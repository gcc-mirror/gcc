/* Write the GIMPLE representation to a file stream.

   Copyright (C) 2009-2016 Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>
   Re-implemented by Diego Novillo <dnovillo@google.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-streamer.h"
#include "alias.h"
#include "stor-layout.h"
#include "gimple-iterator.h"
#include "except.h"
#include "lto-symtab.h"
#include "cgraph.h"
#include "cfgloop.h"
#include "builtins.h"
#include "gomp-constants.h"


static void lto_write_tree (struct output_block*, tree, bool);

/* Clear the line info stored in DATA_IN.  */

static void
clear_line_info (struct output_block *ob)
{
  ob->current_file = NULL;
  ob->current_line = 0;
  ob->current_col = 0;
  ob->current_sysp = false;
}


/* Create the output block and return it.  SECTION_TYPE is
   LTO_section_function_body or LTO_static_initializer.  */

struct output_block *
create_output_block (enum lto_section_type section_type)
{
  struct output_block *ob = XCNEW (struct output_block);

  ob->section_type = section_type;
  ob->decl_state = lto_get_out_decl_state ();
  ob->main_stream = XCNEW (struct lto_output_stream);
  ob->string_stream = XCNEW (struct lto_output_stream);
  ob->writer_cache = streamer_tree_cache_create (!flag_wpa, true, false);

  if (section_type == LTO_section_function_body)
    ob->cfg_stream = XCNEW (struct lto_output_stream);

  clear_line_info (ob);

  ob->string_hash_table = new hash_table<string_slot_hasher> (37);
  gcc_obstack_init (&ob->obstack);

  return ob;
}


/* Destroy the output block OB.  */

void
destroy_output_block (struct output_block *ob)
{
  enum lto_section_type section_type = ob->section_type;

  delete ob->string_hash_table;
  ob->string_hash_table = NULL;

  free (ob->main_stream);
  free (ob->string_stream);
  if (section_type == LTO_section_function_body)
    free (ob->cfg_stream);

  streamer_tree_cache_delete (ob->writer_cache);
  obstack_free (&ob->obstack, NULL);

  free (ob);
}


/* Look up NODE in the type table and write the index for it to OB.  */

static void
output_type_ref (struct output_block *ob, tree node)
{
  streamer_write_record_start (ob, LTO_type_ref);
  lto_output_type_ref_index (ob->decl_state, ob->main_stream, node);
}


/* Return true if tree node T is written to various tables.  For these
   nodes, we sometimes want to write their phyiscal representation
   (via lto_output_tree), and sometimes we need to emit an index
   reference into a table (via lto_output_tree_ref).  */

static bool
tree_is_indexable (tree t)
{
  /* Parameters and return values of functions of variably modified types
     must go to global stream, because they may be used in the type
     definition.  */
  if ((TREE_CODE (t) == PARM_DECL || TREE_CODE (t) == RESULT_DECL)
      && DECL_CONTEXT (t))
    return variably_modified_type_p (TREE_TYPE (DECL_CONTEXT (t)), NULL_TREE);
  /* IMPORTED_DECL is put into BLOCK and thus it never can be shared.  */
  else if (TREE_CODE (t) == IMPORTED_DECL)
    return false;
  else if (((VAR_P (t) && !TREE_STATIC (t))
	    || TREE_CODE (t) == TYPE_DECL
	    || TREE_CODE (t) == CONST_DECL
	    || TREE_CODE (t) == NAMELIST_DECL)
	   && decl_function_context (t))
    return false;
  else if (TREE_CODE (t) == DEBUG_EXPR_DECL)
    return false;
  /* Variably modified types need to be streamed alongside function
     bodies because they can refer to local entities.  Together with
     them we have to localize their members as well.
     ???  In theory that includes non-FIELD_DECLs as well.  */
  else if (TYPE_P (t)
	   && variably_modified_type_p (t, NULL_TREE))
    return false;
  else if (TREE_CODE (t) == FIELD_DECL
	   && variably_modified_type_p (DECL_CONTEXT (t), NULL_TREE))
    return false;
  else
    return (TYPE_P (t) || DECL_P (t) || TREE_CODE (t) == SSA_NAME);
}


/* Output info about new location into bitpack BP.
   After outputting bitpack, lto_output_location_data has
   to be done to output actual data.  */

void
lto_output_location (struct output_block *ob, struct bitpack_d *bp,
		     location_t loc)
{
  expanded_location xloc;

  loc = LOCATION_LOCUS (loc);
  bp_pack_int_in_range (bp, 0, RESERVED_LOCATION_COUNT,
		        loc < RESERVED_LOCATION_COUNT
			? loc : RESERVED_LOCATION_COUNT);
  if (loc < RESERVED_LOCATION_COUNT)
    return;

  xloc = expand_location (loc);

  bp_pack_value (bp, ob->current_file != xloc.file, 1);
  bp_pack_value (bp, ob->current_line != xloc.line, 1);
  bp_pack_value (bp, ob->current_col != xloc.column, 1);

  if (ob->current_file != xloc.file)
    {
      bp_pack_string (ob, bp, xloc.file, true);
      bp_pack_value (bp, xloc.sysp, 1);
    }
  ob->current_file = xloc.file;
  ob->current_sysp = xloc.sysp;

  if (ob->current_line != xloc.line)
    bp_pack_var_len_unsigned (bp, xloc.line);
  ob->current_line = xloc.line;

  if (ob->current_col != xloc.column)
    bp_pack_var_len_unsigned (bp, xloc.column);
  ob->current_col = xloc.column;
}


/* If EXPR is an indexable tree node, output a reference to it to
   output block OB.  Otherwise, output the physical representation of
   EXPR to OB.  */

static void
lto_output_tree_ref (struct output_block *ob, tree expr)
{
  enum tree_code code;

  if (TYPE_P (expr))
    {
      output_type_ref (ob, expr);
      return;
    }

  code = TREE_CODE (expr);
  switch (code)
    {
    case SSA_NAME:
      streamer_write_record_start (ob, LTO_ssa_name_ref);
      streamer_write_uhwi (ob, SSA_NAME_VERSION (expr));
      break;

    case FIELD_DECL:
      streamer_write_record_start (ob, LTO_field_decl_ref);
      lto_output_field_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case FUNCTION_DECL:
      streamer_write_record_start (ob, LTO_function_decl_ref);
      lto_output_fn_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case VAR_DECL:
    case DEBUG_EXPR_DECL:
      gcc_assert (decl_function_context (expr) == NULL || TREE_STATIC (expr));
      /* FALLTHRU */
    case PARM_DECL:
      streamer_write_record_start (ob, LTO_global_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case CONST_DECL:
      streamer_write_record_start (ob, LTO_const_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case IMPORTED_DECL:
      gcc_assert (decl_function_context (expr) == NULL);
      streamer_write_record_start (ob, LTO_imported_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case TYPE_DECL:
      streamer_write_record_start (ob, LTO_type_decl_ref);
      lto_output_type_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case NAMELIST_DECL:
      streamer_write_record_start (ob, LTO_namelist_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case NAMESPACE_DECL:
      streamer_write_record_start (ob, LTO_namespace_decl_ref);
      lto_output_namespace_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case LABEL_DECL:
      streamer_write_record_start (ob, LTO_label_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case RESULT_DECL:
      streamer_write_record_start (ob, LTO_result_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case TRANSLATION_UNIT_DECL:
      streamer_write_record_start (ob, LTO_translation_unit_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    default:
      /* No other node is indexable, so it should have been handled by
	 lto_output_tree.  */
      gcc_unreachable ();
    }
}


/* Return true if EXPR is a tree node that can be written to disk.  */

static inline bool
lto_is_streamable (tree expr)
{
  enum tree_code code = TREE_CODE (expr);

  /* Notice that we reject SSA_NAMEs as well.  We only emit the SSA
     name version in lto_output_tree_ref (see output_ssa_names).  */
  return !is_lang_specific (expr)
	 && code != SSA_NAME
	 && code != CALL_EXPR
	 && code != LANG_TYPE
	 && code != MODIFY_EXPR
	 && code != INIT_EXPR
	 && code != TARGET_EXPR
	 && code != BIND_EXPR
	 && code != WITH_CLEANUP_EXPR
	 && code != STATEMENT_LIST
	 && (code == CASE_LABEL_EXPR
	     || code == DECL_EXPR
	     || TREE_CODE_CLASS (code) != tcc_statement);
}

/* Very rough estimate of streaming size of the initializer.  If we ignored
   presence of strings, we could simply just count number of non-indexable
   tree nodes and number of references to indexable nodes.  Strings however
   may be very large and we do not want to dump them int othe global stream.

   Count the size of initializer until the size in DATA is positive.  */

static tree
subtract_estimated_size (tree *tp, int *ws, void *data)
{
  long *sum = (long *)data;
  if (tree_is_indexable (*tp))
    {
      /* Indexable tree is one reference to global stream.
	 Guess it may be about 4 bytes.  */
      *sum -= 4;
      *ws = 0;
    }
  /* String table entry + base of tree node needs to be streamed.  */
  if (TREE_CODE (*tp) == STRING_CST)
    *sum -= TREE_STRING_LENGTH (*tp) + 8;
  else
    {
      /* Identifiers are also variable length but should not appear
	 naked in constructor.  */
      gcc_checking_assert (TREE_CODE (*tp) != IDENTIFIER_NODE);
      /* We do not really make attempt to work out size of pickled tree, as
	 it is very variable. Make it bigger than the reference.  */
      *sum -= 16;
    }
  if (*sum < 0)
    return *tp;
  return NULL_TREE;
}


/* For EXPR lookup and return what we want to stream to OB as DECL_INITIAL.  */

static tree
get_symbol_initial_value (lto_symtab_encoder_t encoder, tree expr)
{
  gcc_checking_assert (DECL_P (expr)
		       && TREE_CODE (expr) != FUNCTION_DECL
		       && TREE_CODE (expr) != TRANSLATION_UNIT_DECL);

  /* Handle DECL_INITIAL for symbols.  */
  tree initial = DECL_INITIAL (expr);
  if (VAR_P (expr)
      && (TREE_STATIC (expr) || DECL_EXTERNAL (expr))
      && !DECL_IN_CONSTANT_POOL (expr)
      && initial)
    {
      varpool_node *vnode;
      /* Extra section needs about 30 bytes; do not produce it for simple
	 scalar values.  */
      if (!(vnode = varpool_node::get (expr))
	  || !lto_symtab_encoder_encode_initializer_p (encoder, vnode))
        initial = error_mark_node;
      if (initial != error_mark_node)
	{
	  long max_size = 30;
	  if (walk_tree (&initial, subtract_estimated_size, (void *)&max_size,
			 NULL))
	    initial = error_mark_node;
	}
    }

  return initial;
}


/* Write a physical representation of tree node EXPR to output block
   OB.  If REF_P is true, the leaves of EXPR are emitted as references
   via lto_output_tree_ref.  IX is the index into the streamer cache
   where EXPR is stored.  */

static void
lto_write_tree_1 (struct output_block *ob, tree expr, bool ref_p)
{
  /* Pack all the non-pointer fields in EXPR into a bitpack and write
     the resulting bitpack.  */
  streamer_write_tree_bitfields (ob, expr);

  /* Write all the pointer fields in EXPR.  */
  streamer_write_tree_body (ob, expr, ref_p);

  /* Write any LTO-specific data to OB.  */
  if (DECL_P (expr)
      && TREE_CODE (expr) != FUNCTION_DECL
      && TREE_CODE (expr) != TRANSLATION_UNIT_DECL)
    {
      /* Handle DECL_INITIAL for symbols.  */
      tree initial = get_symbol_initial_value
			 (ob->decl_state->symtab_node_encoder, expr);
      stream_write_tree (ob, initial, ref_p);
    }
}

/* Write a physical representation of tree node EXPR to output block
   OB.  If REF_P is true, the leaves of EXPR are emitted as references
   via lto_output_tree_ref.  IX is the index into the streamer cache
   where EXPR is stored.  */

static void
lto_write_tree (struct output_block *ob, tree expr, bool ref_p)
{
  if (!lto_is_streamable (expr))
    internal_error ("tree code %qs is not supported in LTO streams",
		    get_tree_code_name (TREE_CODE (expr)));

  /* Write the header, containing everything needed to materialize
     EXPR on the reading side.  */
  streamer_write_tree_header (ob, expr);

  lto_write_tree_1 (ob, expr, ref_p);

  /* Mark the end of EXPR.  */
  streamer_write_zero (ob);
}

/* Emit the physical representation of tree node EXPR to output block OB,
   If THIS_REF_P is true, the leaves of EXPR are emitted as references via
   lto_output_tree_ref.  REF_P is used for streaming siblings of EXPR.  */

static void
lto_output_tree_1 (struct output_block *ob, tree expr, hashval_t hash,
		   bool ref_p, bool this_ref_p)
{
  unsigned ix;

  gcc_checking_assert (expr != NULL_TREE
		       && !(this_ref_p && tree_is_indexable (expr)));

  bool exists_p = streamer_tree_cache_insert (ob->writer_cache,
					      expr, hash, &ix);
  gcc_assert (!exists_p);
  if (TREE_CODE (expr) == INTEGER_CST
      && !TREE_OVERFLOW (expr))
    {
      /* Shared INTEGER_CST nodes are special because they need their
	 original type to be materialized by the reader (to implement
	 TYPE_CACHED_VALUES).  */
      streamer_write_integer_cst (ob, expr, ref_p);
    }
  else
    {
      /* This is the first time we see EXPR, write its fields
	 to OB.  */
      lto_write_tree (ob, expr, ref_p);
    }
}

class DFS
{
public:
  DFS (struct output_block *ob, tree expr, bool ref_p, bool this_ref_p,
       bool single_p);
  ~DFS ();

  struct scc_entry
  {
    tree t;
    hashval_t hash;
  };
  vec<scc_entry> sccstack;

private:
  struct sccs
  {
    unsigned int dfsnum;
    unsigned int low;
  };
  struct worklist
  {
    tree expr;
    sccs *from_state;
    sccs *cstate;
    bool ref_p;
    bool this_ref_p;
  };

  static int scc_entry_compare (const void *, const void *);

  void DFS_write_tree_body (struct output_block *ob,
			    tree expr, sccs *expr_state, bool ref_p);

  void DFS_write_tree (struct output_block *ob, sccs *from_state,
		       tree expr, bool ref_p, bool this_ref_p);

  hashval_t
  hash_scc (struct output_block *ob, unsigned first, unsigned size,
	    bool ref_p, bool this_ref_p);

  hash_map<tree, sccs *> sccstate;
  vec<worklist> worklist_vec;
  struct obstack sccstate_obstack;
};

/* Emit the physical representation of tree node EXPR to output block OB,
   using depth-first search on the subgraph.  If THIS_REF_P is true, the
   leaves of EXPR are emitted as references via lto_output_tree_ref.
   REF_P is used for streaming siblings of EXPR.  If SINGLE_P is true,
   this is for a rewalk of a single leaf SCC.  */

DFS::DFS (struct output_block *ob, tree expr, bool ref_p, bool this_ref_p,
	  bool single_p)
{
  unsigned int next_dfs_num = 1;
  sccstack.create (0);
  gcc_obstack_init (&sccstate_obstack);
  worklist_vec = vNULL;
  DFS_write_tree (ob, NULL, expr, ref_p, this_ref_p);
  while (!worklist_vec.is_empty ())
    {
      worklist &w = worklist_vec.last ();
      expr = w.expr;
      sccs *from_state = w.from_state;
      sccs *cstate = w.cstate;
      ref_p = w.ref_p;
      this_ref_p = w.this_ref_p;
      if (cstate == NULL)
	{
	  sccs **slot = &sccstate.get_or_insert (expr);
	  cstate = *slot;
	  if (cstate)
	    {
	      gcc_checking_assert (from_state);
	      if (cstate->dfsnum < from_state->dfsnum)
		from_state->low = MIN (cstate->dfsnum, from_state->low);
	      worklist_vec.pop ();
	      continue;
	    }

	  scc_entry e = { expr, 0 };
	  /* Not yet visited.  DFS recurse and push it onto the stack.  */
	  *slot = cstate = XOBNEW (&sccstate_obstack, struct sccs);
	  sccstack.safe_push (e);
	  cstate->dfsnum = next_dfs_num++;
	  cstate->low = cstate->dfsnum;
	  w.cstate = cstate;

	  if (TREE_CODE (expr) == INTEGER_CST
	      && !TREE_OVERFLOW (expr))
	    DFS_write_tree (ob, cstate, TREE_TYPE (expr), ref_p, ref_p);
	  else
	    {
	      DFS_write_tree_body (ob, expr, cstate, ref_p);

	      /* Walk any LTO-specific edges.  */
	      if (DECL_P (expr)
		  && TREE_CODE (expr) != FUNCTION_DECL
		  && TREE_CODE (expr) != TRANSLATION_UNIT_DECL)
		{
		  /* Handle DECL_INITIAL for symbols.  */
		  tree initial
		    = get_symbol_initial_value (ob->decl_state->symtab_node_encoder,
						expr);
		  DFS_write_tree (ob, cstate, initial, ref_p, ref_p);
		}
	    }
	  continue;
	}

      /* See if we found an SCC.  */
      if (cstate->low == cstate->dfsnum)
	{
	  unsigned first, size;
	  tree x;

	  /* If we are re-walking a single leaf SCC just pop it,
	     let earlier worklist item access the sccstack.  */
	  if (single_p)
	    {
	      worklist_vec.pop ();
	      continue;
	    }

	  /* Pop the SCC and compute its size.  */
	  first = sccstack.length ();
	  do
	    {
	      x = sccstack[--first].t;
	    }
	  while (x != expr);
	  size = sccstack.length () - first;

	  /* No need to compute hashes for LTRANS units, we don't perform
	     any merging there.  */
	  hashval_t scc_hash = 0;
	  unsigned scc_entry_len = 0;
	  if (!flag_wpa)
	    {
	      scc_hash = hash_scc (ob, first, size, ref_p, this_ref_p);

	      /* Put the entries with the least number of collisions first.  */
	      unsigned entry_start = 0;
	      scc_entry_len = size + 1;
	      for (unsigned i = 0; i < size;)
		{
		  unsigned from = i;
		  for (i = i + 1; i < size
		       && (sccstack[first + i].hash
			   == sccstack[first + from].hash); ++i)
		    ;
		  if (i - from < scc_entry_len)
		    {
		      scc_entry_len = i - from;
		      entry_start = from;
		    }
		}
	      for (unsigned i = 0; i < scc_entry_len; ++i)
		std::swap (sccstack[first + i],
			   sccstack[first + entry_start + i]);

	      /* We already sorted SCC deterministically in hash_scc.  */

	      /* Check that we have only one SCC.
		 Naturally we may have conflicts if hash function is not
		 strong enough.  Lets see how far this gets.  */
	      gcc_checking_assert (scc_entry_len == 1);
	    }

	  /* Write LTO_tree_scc.  */
	  streamer_write_record_start (ob, LTO_tree_scc);
	  streamer_write_uhwi (ob, size);
	  streamer_write_uhwi (ob, scc_hash);

	  /* Write size-1 SCCs without wrapping them inside SCC bundles.
	     All INTEGER_CSTs need to be handled this way as we need
	     their type to materialize them.  Also builtins are handled
	     this way.
	     ???  We still wrap these in LTO_tree_scc so at the
	     input side we can properly identify the tree we want
	     to ultimatively return.  */
	  if (size == 1)
	    lto_output_tree_1 (ob, expr, scc_hash, ref_p, this_ref_p);
	  else
	    {
	      /* Write the size of the SCC entry candidates.  */
	      streamer_write_uhwi (ob, scc_entry_len);

	      /* Write all headers and populate the streamer cache.  */
	      for (unsigned i = 0; i < size; ++i)
		{
		  hashval_t hash = sccstack[first+i].hash;
		  tree t = sccstack[first+i].t;
		  bool exists_p = streamer_tree_cache_insert (ob->writer_cache,
							      t, hash, NULL);
		  gcc_assert (!exists_p);

		  if (!lto_is_streamable (t))
		    internal_error ("tree code %qs is not supported "
				    "in LTO streams",
				    get_tree_code_name (TREE_CODE (t)));

		  /* Write the header, containing everything needed to
		     materialize EXPR on the reading side.  */
		  streamer_write_tree_header (ob, t);
		}

	      /* Write the bitpacks and tree references.  */
	      for (unsigned i = 0; i < size; ++i)
		{
		  lto_write_tree_1 (ob, sccstack[first+i].t, ref_p);

		  /* Mark the end of the tree.  */
		  streamer_write_zero (ob);
		}
	    }

	  /* Finally truncate the vector.  */
	  sccstack.truncate (first);

	  if (from_state)
	    from_state->low = MIN (from_state->low, cstate->low);
	  worklist_vec.pop ();
	  continue;
	}

      gcc_checking_assert (from_state);
      from_state->low = MIN (from_state->low, cstate->low);
      if (cstate->dfsnum < from_state->dfsnum)
	from_state->low = MIN (cstate->dfsnum, from_state->low);
      worklist_vec.pop ();
    }
  worklist_vec.release ();
}

DFS::~DFS ()
{
  sccstack.release ();
  obstack_free (&sccstate_obstack, NULL);
}

/* Handle the tree EXPR in the DFS walk with SCC state EXPR_STATE and
   DFS recurse for all tree edges originating from it.  */

void
DFS::DFS_write_tree_body (struct output_block *ob,
			  tree expr, sccs *expr_state, bool ref_p)
{
#define DFS_follow_tree_edge(DEST) \
  DFS_write_tree (ob, expr_state, DEST, ref_p, ref_p)

  enum tree_code code;

  code = TREE_CODE (expr);

  if (CODE_CONTAINS_STRUCT (code, TS_TYPED))
    {
      if (TREE_CODE (expr) != IDENTIFIER_NODE)
	DFS_follow_tree_edge (TREE_TYPE (expr));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_VECTOR))
    {
      for (unsigned i = 0; i < VECTOR_CST_NELTS (expr); ++i)
	DFS_follow_tree_edge (VECTOR_CST_ELT (expr, i));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_COMPLEX))
    {
      DFS_follow_tree_edge (TREE_REALPART (expr));
      DFS_follow_tree_edge (TREE_IMAGPART (expr));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_MINIMAL))
    {
      /* Drop names that were created for anonymous entities.  */
      if (DECL_NAME (expr)
	  && TREE_CODE (DECL_NAME (expr)) == IDENTIFIER_NODE
	  && anon_aggrname_p (DECL_NAME (expr)))
	;
      else
	DFS_follow_tree_edge (DECL_NAME (expr));
      DFS_follow_tree_edge (DECL_CONTEXT (expr));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    {
      DFS_follow_tree_edge (DECL_SIZE (expr));
      DFS_follow_tree_edge (DECL_SIZE_UNIT (expr));

      /* Note, DECL_INITIAL is not handled here.  Since DECL_INITIAL needs
	 special handling in LTO, it must be handled by streamer hooks.  */

      DFS_follow_tree_edge (DECL_ATTRIBUTES (expr));

      /* Do not follow DECL_ABSTRACT_ORIGIN.  We cannot handle debug information
	 for early inlining so drop it on the floor instead of ICEing in
	 dwarf2out.c.
	 We however use DECL_ABSTRACT_ORIGIN == error_mark_node to mark
	 declarations which should be eliminated by decl merging. Be sure none
	 leaks to this point.  */
      gcc_assert (DECL_ABSTRACT_ORIGIN (expr) != error_mark_node);

      if ((VAR_P (expr)
	   || TREE_CODE (expr) == PARM_DECL)
	  && DECL_HAS_VALUE_EXPR_P (expr))
	DFS_follow_tree_edge (DECL_VALUE_EXPR (expr));
      if (VAR_P (expr))
	DFS_follow_tree_edge (DECL_DEBUG_EXPR (expr));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_NON_COMMON))
    {
      if (TREE_CODE (expr) == TYPE_DECL)
	DFS_follow_tree_edge (DECL_ORIGINAL_TYPE (expr));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    {
      /* Make sure we don't inadvertently set the assembler name.  */
      if (DECL_ASSEMBLER_NAME_SET_P (expr))
	DFS_follow_tree_edge (DECL_ASSEMBLER_NAME (expr));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_FIELD_DECL))
    {
      DFS_follow_tree_edge (DECL_FIELD_OFFSET (expr));
      DFS_follow_tree_edge (DECL_BIT_FIELD_TYPE (expr));
      DFS_follow_tree_edge (DECL_BIT_FIELD_REPRESENTATIVE (expr));
      DFS_follow_tree_edge (DECL_FIELD_BIT_OFFSET (expr));
      DFS_follow_tree_edge (DECL_FCONTEXT (expr));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    {
      DFS_follow_tree_edge (DECL_VINDEX (expr));
      DFS_follow_tree_edge (DECL_FUNCTION_PERSONALITY (expr));
      DFS_follow_tree_edge (DECL_FUNCTION_SPECIFIC_TARGET (expr));
      DFS_follow_tree_edge (DECL_FUNCTION_SPECIFIC_OPTIMIZATION (expr));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    {
      DFS_follow_tree_edge (TYPE_SIZE (expr));
      DFS_follow_tree_edge (TYPE_SIZE_UNIT (expr));
      DFS_follow_tree_edge (TYPE_ATTRIBUTES (expr));
      DFS_follow_tree_edge (TYPE_NAME (expr));
      /* Do not follow TYPE_POINTER_TO or TYPE_REFERENCE_TO.  They will be
	 reconstructed during fixup.  */
      /* Do not follow TYPE_NEXT_VARIANT, we reconstruct the variant lists
	 during fixup.  */
      DFS_follow_tree_edge (TYPE_MAIN_VARIANT (expr));
      DFS_follow_tree_edge (TYPE_CONTEXT (expr));
      /* TYPE_CANONICAL is re-computed during type merging, so no need
	 to follow it here.  */
      DFS_follow_tree_edge (TYPE_STUB_DECL (expr));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_NON_COMMON))
    {
      if (TREE_CODE (expr) == ENUMERAL_TYPE)
	DFS_follow_tree_edge (TYPE_VALUES (expr));
      else if (TREE_CODE (expr) == ARRAY_TYPE)
	DFS_follow_tree_edge (TYPE_DOMAIN (expr));
      else if (RECORD_OR_UNION_TYPE_P (expr))
	for (tree t = TYPE_FIELDS (expr); t; t = TREE_CHAIN (t))
	  DFS_follow_tree_edge (t);
      else if (TREE_CODE (expr) == FUNCTION_TYPE
	       || TREE_CODE (expr) == METHOD_TYPE)
	DFS_follow_tree_edge (TYPE_ARG_TYPES (expr));

      if (!POINTER_TYPE_P (expr))
	DFS_follow_tree_edge (TYPE_MINVAL (expr));
      DFS_follow_tree_edge (TYPE_MAXVAL (expr));
      if (RECORD_OR_UNION_TYPE_P (expr))
	DFS_follow_tree_edge (TYPE_BINFO (expr));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_LIST))
    {
      DFS_follow_tree_edge (TREE_PURPOSE (expr));
      DFS_follow_tree_edge (TREE_VALUE (expr));
      DFS_follow_tree_edge (TREE_CHAIN (expr));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_VEC))
    {
      for (int i = 0; i < TREE_VEC_LENGTH (expr); i++)
	DFS_follow_tree_edge (TREE_VEC_ELT (expr, i));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_EXP))
    {
      for (int i = 0; i < TREE_OPERAND_LENGTH (expr); i++)
	DFS_follow_tree_edge (TREE_OPERAND (expr, i));
      DFS_follow_tree_edge (TREE_BLOCK (expr));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_BLOCK))
    {
      for (tree t = BLOCK_VARS (expr); t; t = TREE_CHAIN (t))
	if (VAR_OR_FUNCTION_DECL_P (t)
	    && DECL_EXTERNAL (t))
	  /* We have to stream externals in the block chain as
	     non-references.  See also
	     tree-streamer-out.c:streamer_write_chain.  */
	  DFS_write_tree (ob, expr_state, t, ref_p, false);
	else
	  DFS_follow_tree_edge (t);

      DFS_follow_tree_edge (BLOCK_SUPERCONTEXT (expr));

      /* Follow BLOCK_ABSTRACT_ORIGIN for the limited cases we can
	 handle - those that represent inlined function scopes.
	 For the drop rest them on the floor instead of ICEing
	 in dwarf2out.c, but keep the notion of whether the block
	 is an inlined block by refering to itself for the sake of
	 tree_nonartificial_location.  */
      if (inlined_function_outer_scope_p (expr))
	{
	  tree ultimate_origin = block_ultimate_origin (expr);
	  DFS_follow_tree_edge (ultimate_origin);
	}
      else if (BLOCK_ABSTRACT_ORIGIN (expr))
	DFS_follow_tree_edge (expr);
      /* Do not follow BLOCK_NONLOCALIZED_VARS.  We cannot handle debug
	 information for early inlined BLOCKs so drop it on the floor instead
	 of ICEing in dwarf2out.c.  */

      /* BLOCK_FRAGMENT_ORIGIN and BLOCK_FRAGMENT_CHAIN is not live at LTO
	 streaming time.  */

      /* Do not output BLOCK_SUBBLOCKS.  Instead on streaming-in this
	 list is re-constructed from BLOCK_SUPERCONTEXT.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    {
      unsigned i;
      tree t;

      /* Note that the number of BINFO slots has already been emitted in
	 EXPR's header (see streamer_write_tree_header) because this length
	 is needed to build the empty BINFO node on the reader side.  */
      FOR_EACH_VEC_ELT (*BINFO_BASE_BINFOS (expr), i, t)
	DFS_follow_tree_edge (t);
      DFS_follow_tree_edge (BINFO_OFFSET (expr));
      DFS_follow_tree_edge (BINFO_VTABLE (expr));
      DFS_follow_tree_edge (BINFO_VPTR_FIELD (expr));

      /* The number of BINFO_BASE_ACCESSES has already been emitted in
	 EXPR's bitfield section.  */
      FOR_EACH_VEC_SAFE_ELT (BINFO_BASE_ACCESSES (expr), i, t)
	DFS_follow_tree_edge (t);

      /* Do not walk BINFO_INHERITANCE_CHAIN, BINFO_SUBVTT_INDEX
	 and BINFO_VPTR_INDEX; these are used by C++ FE only.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_CONSTRUCTOR))
    {
      unsigned i;
      tree index, value;

      FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (expr), i, index, value)
	{
	  DFS_follow_tree_edge (index);
	  DFS_follow_tree_edge (value);
	}
    }

  if (code == OMP_CLAUSE)
    {
      int i;
      for (i = 0; i < omp_clause_num_ops[OMP_CLAUSE_CODE (expr)]; i++)
	DFS_follow_tree_edge (OMP_CLAUSE_OPERAND (expr, i));
      DFS_follow_tree_edge (OMP_CLAUSE_CHAIN (expr));
    }

#undef DFS_follow_tree_edge
}

/* Return a hash value for the tree T.
   CACHE holds hash values of trees outside current SCC.  MAP, if non-NULL,
   may hold hash values if trees inside current SCC.  */

static hashval_t
hash_tree (struct streamer_tree_cache_d *cache, hash_map<tree, hashval_t> *map, tree t)
{
  inchash::hash hstate;

#define visit(SIBLING) \
  do { \
    unsigned ix; \
    if (!SIBLING) \
      hstate.add_int (0); \
    else if (streamer_tree_cache_lookup (cache, SIBLING, &ix)) \
      hstate.add_int (streamer_tree_cache_get_hash (cache, ix)); \
    else if (map) \
      hstate.add_int (*map->get (SIBLING)); \
    else \
      hstate.add_int (1); \
  } while (0)

  /* Hash TS_BASE.  */
  enum tree_code code = TREE_CODE (t);
  hstate.add_int (code);
  if (!TYPE_P (t))
    {
      hstate.add_flag (TREE_SIDE_EFFECTS (t));
      hstate.add_flag (TREE_CONSTANT (t));
      hstate.add_flag (TREE_READONLY (t));
      hstate.add_flag (TREE_PUBLIC (t));
    }
  hstate.add_flag (TREE_ADDRESSABLE (t));
  hstate.add_flag (TREE_THIS_VOLATILE (t));
  if (DECL_P (t))
    hstate.add_flag (DECL_UNSIGNED (t));
  else if (TYPE_P (t))
    hstate.add_flag (TYPE_UNSIGNED (t));
  if (TYPE_P (t))
    hstate.add_flag (TYPE_ARTIFICIAL (t));
  else
    hstate.add_flag (TREE_NO_WARNING (t));
  hstate.add_flag (TREE_NOTHROW (t));
  hstate.add_flag (TREE_STATIC (t));
  hstate.add_flag (TREE_PROTECTED (t));
  hstate.add_flag (TREE_DEPRECATED (t));
  if (code != TREE_BINFO)
    hstate.add_flag (TREE_PRIVATE (t));
  if (TYPE_P (t))
    {
      hstate.add_flag (AGGREGATE_TYPE_P (t)
		       ? TYPE_REVERSE_STORAGE_ORDER (t) : TYPE_SATURATING (t));
      hstate.add_flag (TYPE_ADDR_SPACE (t));
    }
  else if (code == SSA_NAME)
    hstate.add_flag (SSA_NAME_IS_DEFAULT_DEF (t));
  hstate.commit_flag ();

  if (CODE_CONTAINS_STRUCT (code, TS_INT_CST))
    {
      int i;
      hstate.add_wide_int (TREE_INT_CST_NUNITS (t));
      hstate.add_wide_int (TREE_INT_CST_EXT_NUNITS (t));
      for (i = 0; i < TREE_INT_CST_NUNITS (t); i++)
	hstate.add_wide_int (TREE_INT_CST_ELT (t, i));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_REAL_CST))
    {
      REAL_VALUE_TYPE r = TREE_REAL_CST (t);
      hstate.add_flag (r.cl);
      hstate.add_flag (r.sign);
      hstate.add_flag (r.signalling);
      hstate.add_flag (r.canonical);
      hstate.commit_flag ();
      hstate.add_int (r.uexp);
      hstate.add (r.sig, sizeof (r.sig));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_FIXED_CST))
    {
      FIXED_VALUE_TYPE f = TREE_FIXED_CST (t);
      hstate.add_int (f.mode);
      hstate.add_int (f.data.low);
      hstate.add_int (f.data.high);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    {
      hstate.add_wide_int (DECL_MODE (t));
      hstate.add_flag (DECL_NONLOCAL (t));
      hstate.add_flag (DECL_VIRTUAL_P (t));
      hstate.add_flag (DECL_IGNORED_P (t));
      hstate.add_flag (DECL_ABSTRACT_P (t));
      hstate.add_flag (DECL_ARTIFICIAL (t));
      hstate.add_flag (DECL_USER_ALIGN (t));
      hstate.add_flag (DECL_PRESERVE_P (t));
      hstate.add_flag (DECL_EXTERNAL (t));
      hstate.add_flag (DECL_GIMPLE_REG_P (t));
      hstate.commit_flag ();
      hstate.add_int (DECL_ALIGN (t));
      if (code == LABEL_DECL)
	{
          hstate.add_int (EH_LANDING_PAD_NR (t));
	  hstate.add_int (LABEL_DECL_UID (t));
	}
      else if (code == FIELD_DECL)
	{
	  hstate.add_flag (DECL_PACKED (t));
	  hstate.add_flag (DECL_NONADDRESSABLE_P (t));
	  hstate.add_int (DECL_OFFSET_ALIGN (t));
	}
      else if (code == VAR_DECL)
	{
	  hstate.add_flag (DECL_HAS_DEBUG_EXPR_P (t));
	  hstate.add_flag (DECL_NONLOCAL_FRAME (t));
	}
      if (code == RESULT_DECL
	  || code == PARM_DECL
	  || code == VAR_DECL)
	{
	  hstate.add_flag (DECL_BY_REFERENCE (t));
	  if (code == VAR_DECL
	      || code == PARM_DECL)
	    hstate.add_flag (DECL_HAS_VALUE_EXPR_P (t));
	}
      hstate.commit_flag ();
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WRTL))
    hstate.add_int (DECL_REGISTER (t));

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    {
      hstate.add_flag (DECL_COMMON (t));
      hstate.add_flag (DECL_DLLIMPORT_P (t));
      hstate.add_flag (DECL_WEAK (t));
      hstate.add_flag (DECL_SEEN_IN_BIND_EXPR_P (t));
      hstate.add_flag (DECL_COMDAT (t));
      hstate.add_flag (DECL_VISIBILITY_SPECIFIED (t));
      hstate.add_int (DECL_VISIBILITY (t));
      if (code == VAR_DECL)
	{
	  /* DECL_IN_TEXT_SECTION is set during final asm output only.  */
	  hstate.add_flag (DECL_HARD_REGISTER (t));
	  hstate.add_flag (DECL_IN_CONSTANT_POOL (t));
	}
      if (TREE_CODE (t) == FUNCTION_DECL)
        {
	  hstate.add_flag (DECL_FINAL_P (t));
	  hstate.add_flag (DECL_CXX_CONSTRUCTOR_P (t));
	  hstate.add_flag (DECL_CXX_DESTRUCTOR_P (t));
	}
      hstate.commit_flag ();
    }

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    {
      hstate.add_int (DECL_BUILT_IN_CLASS (t));
      hstate.add_flag (DECL_STATIC_CONSTRUCTOR (t));
      hstate.add_flag (DECL_STATIC_DESTRUCTOR (t));
      hstate.add_flag (DECL_UNINLINABLE (t));
      hstate.add_flag (DECL_POSSIBLY_INLINED (t));
      hstate.add_flag (DECL_IS_NOVOPS (t));
      hstate.add_flag (DECL_IS_RETURNS_TWICE (t));
      hstate.add_flag (DECL_IS_MALLOC (t));
      hstate.add_flag (DECL_IS_OPERATOR_NEW (t));
      hstate.add_flag (DECL_DECLARED_INLINE_P (t));
      hstate.add_flag (DECL_STATIC_CHAIN (t));
      hstate.add_flag (DECL_NO_INLINE_WARNING_P (t));
      hstate.add_flag (DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (t));
      hstate.add_flag (DECL_NO_LIMIT_STACK (t));
      hstate.add_flag (DECL_DISREGARD_INLINE_LIMITS (t));
      hstate.add_flag (DECL_PURE_P (t));
      hstate.add_flag (DECL_LOOPING_CONST_OR_PURE_P (t));
      hstate.commit_flag ();
      if (DECL_BUILT_IN_CLASS (t) != NOT_BUILT_IN)
	hstate.add_int (DECL_FUNCTION_CODE (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    {
      hstate.add_wide_int (TYPE_MODE (t));
      hstate.add_flag (TYPE_STRING_FLAG (t));
      /* TYPE_NO_FORCE_BLK is private to stor-layout and need
 	 no streaming.  */
      hstate.add_flag (TYPE_NEEDS_CONSTRUCTING (t));
      hstate.add_flag (TYPE_PACKED (t));
      hstate.add_flag (TYPE_RESTRICT (t));
      hstate.add_flag (TYPE_USER_ALIGN (t));
      hstate.add_flag (TYPE_READONLY (t));
      if (RECORD_OR_UNION_TYPE_P (t))
	{
	  hstate.add_flag (TYPE_TRANSPARENT_AGGR (t));
	  hstate.add_flag (TYPE_FINAL_P (t));
	}
      else if (code == ARRAY_TYPE)
	hstate.add_flag (TYPE_NONALIASED_COMPONENT (t));
      hstate.commit_flag ();
      hstate.add_int (TYPE_PRECISION (t));
      hstate.add_int (TYPE_ALIGN (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TRANSLATION_UNIT_DECL))
    hstate.add (TRANSLATION_UNIT_LANGUAGE (t),
			strlen (TRANSLATION_UNIT_LANGUAGE (t)));

  if (CODE_CONTAINS_STRUCT (code, TS_TARGET_OPTION)
      /* We don't stream these when passing things to a different target.  */
      && !lto_stream_offload_p)
    hstate.add_wide_int (cl_target_option_hash (TREE_TARGET_OPTION (t)));

  if (CODE_CONTAINS_STRUCT (code, TS_OPTIMIZATION))
    hstate.add_wide_int (cl_optimization_hash (TREE_OPTIMIZATION (t)));

  if (CODE_CONTAINS_STRUCT (code, TS_IDENTIFIER))
    hstate.merge_hash (IDENTIFIER_HASH_VALUE (t));

  if (CODE_CONTAINS_STRUCT (code, TS_STRING))
    hstate.add (TREE_STRING_POINTER (t), TREE_STRING_LENGTH (t));

  if (CODE_CONTAINS_STRUCT (code, TS_TYPED))
    {
      if (code != IDENTIFIER_NODE)
	visit (TREE_TYPE (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_VECTOR))
    for (unsigned i = 0; i < VECTOR_CST_NELTS (t); ++i)
      visit (VECTOR_CST_ELT (t, i));

  if (CODE_CONTAINS_STRUCT (code, TS_COMPLEX))
    {
      visit (TREE_REALPART (t));
      visit (TREE_IMAGPART (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_MINIMAL))
    {
      /* Drop names that were created for anonymous entities.  */
      if (DECL_NAME (t)
	  && TREE_CODE (DECL_NAME (t)) == IDENTIFIER_NODE
	  && anon_aggrname_p (DECL_NAME (t)))
	;
      else
	visit (DECL_NAME (t));
      if (DECL_FILE_SCOPE_P (t))
	;
      else
	visit (DECL_CONTEXT (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    {
      visit (DECL_SIZE (t));
      visit (DECL_SIZE_UNIT (t));
      visit (DECL_ATTRIBUTES (t));
      if ((code == VAR_DECL
	   || code == PARM_DECL)
	  && DECL_HAS_VALUE_EXPR_P (t))
	visit (DECL_VALUE_EXPR (t));
      if (code == VAR_DECL
	  && DECL_HAS_DEBUG_EXPR_P (t))
	visit (DECL_DEBUG_EXPR (t));
      /* ???  Hash DECL_INITIAL as streamed.  Needs the output-block to
         be able to call get_symbol_initial_value.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_NON_COMMON))
    {
      if (code == TYPE_DECL)
	visit (DECL_ORIGINAL_TYPE (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    {
      if (DECL_ASSEMBLER_NAME_SET_P (t))
	visit (DECL_ASSEMBLER_NAME (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_FIELD_DECL))
    {
      visit (DECL_FIELD_OFFSET (t));
      visit (DECL_BIT_FIELD_TYPE (t));
      visit (DECL_BIT_FIELD_REPRESENTATIVE (t));
      visit (DECL_FIELD_BIT_OFFSET (t));
      visit (DECL_FCONTEXT (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    {
      visit (DECL_VINDEX (t));
      visit (DECL_FUNCTION_PERSONALITY (t));
      visit (DECL_FUNCTION_SPECIFIC_TARGET (t));
      visit (DECL_FUNCTION_SPECIFIC_OPTIMIZATION (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    {
      visit (TYPE_SIZE (t));
      visit (TYPE_SIZE_UNIT (t));
      visit (TYPE_ATTRIBUTES (t));
      visit (TYPE_NAME (t));
      visit (TYPE_MAIN_VARIANT (t));
      if (TYPE_FILE_SCOPE_P (t))
	;
      else
	visit (TYPE_CONTEXT (t));
      visit (TYPE_STUB_DECL (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_NON_COMMON))
    {
      if (code == ENUMERAL_TYPE)
	visit (TYPE_VALUES (t));
      else if (code == ARRAY_TYPE)
	visit (TYPE_DOMAIN (t));
      else if (RECORD_OR_UNION_TYPE_P (t))
	for (tree f = TYPE_FIELDS (t); f; f = TREE_CHAIN (f))
	  visit (f);
      else if (code == FUNCTION_TYPE
	       || code == METHOD_TYPE)
	visit (TYPE_ARG_TYPES (t));
      if (!POINTER_TYPE_P (t))
	visit (TYPE_MINVAL (t));
      visit (TYPE_MAXVAL (t));
      if (RECORD_OR_UNION_TYPE_P (t))
	visit (TYPE_BINFO (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_LIST))
    {
      visit (TREE_PURPOSE (t));
      visit (TREE_VALUE (t));
      visit (TREE_CHAIN (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_VEC))
    for (int i = 0; i < TREE_VEC_LENGTH (t); ++i)
      visit (TREE_VEC_ELT (t, i));

  if (CODE_CONTAINS_STRUCT (code, TS_EXP))
    {
      hstate.add_wide_int (TREE_OPERAND_LENGTH (t));
      for (int i = 0; i < TREE_OPERAND_LENGTH (t); ++i)
	visit (TREE_OPERAND (t, i));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    {
      unsigned i;
      tree b;
      FOR_EACH_VEC_ELT (*BINFO_BASE_BINFOS (t), i, b)
	visit (b);
      visit (BINFO_OFFSET (t));
      visit (BINFO_VTABLE (t));
      visit (BINFO_VPTR_FIELD (t));
      FOR_EACH_VEC_SAFE_ELT (BINFO_BASE_ACCESSES (t), i, b)
	visit (b);
      /* Do not walk BINFO_INHERITANCE_CHAIN, BINFO_SUBVTT_INDEX
	 and BINFO_VPTR_INDEX; these are used by C++ FE only.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_CONSTRUCTOR))
    {
      unsigned i;
      tree index, value;
      hstate.add_wide_int (CONSTRUCTOR_NELTS (t));
      FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (t), i, index, value)
	{
	  visit (index);
	  visit (value);
	}
    }

  if (code == OMP_CLAUSE)
    {
      int i;
      HOST_WIDE_INT val;

      hstate.add_wide_int (OMP_CLAUSE_CODE (t));
      switch (OMP_CLAUSE_CODE (t))
	{
	case OMP_CLAUSE_DEFAULT:
	  val = OMP_CLAUSE_DEFAULT_KIND (t);
	  break;
	case OMP_CLAUSE_SCHEDULE:
	  val = OMP_CLAUSE_SCHEDULE_KIND (t);
	  break;
	case OMP_CLAUSE_DEPEND:
	  val = OMP_CLAUSE_DEPEND_KIND (t);
	  break;
	case OMP_CLAUSE_MAP:
	  val = OMP_CLAUSE_MAP_KIND (t);
	  break;
	case OMP_CLAUSE_PROC_BIND:
	  val = OMP_CLAUSE_PROC_BIND_KIND (t);
	  break;
	case OMP_CLAUSE_REDUCTION:
	  val = OMP_CLAUSE_REDUCTION_CODE (t);
	  break;
	default:
	  val = 0;
	  break;
	}
      hstate.add_wide_int (val);
      for (i = 0; i < omp_clause_num_ops[OMP_CLAUSE_CODE (t)]; i++)
	visit (OMP_CLAUSE_OPERAND (t, i));
      visit (OMP_CLAUSE_CHAIN (t));
    }

  return hstate.end ();

#undef visit
}

/* Compare two SCC entries by their hash value for qsorting them.  */

int
DFS::scc_entry_compare (const void *p1_, const void *p2_)
{
  const scc_entry *p1 = (const scc_entry *) p1_;
  const scc_entry *p2 = (const scc_entry *) p2_;
  if (p1->hash < p2->hash)
    return -1;
  else if (p1->hash > p2->hash)
    return 1;
  return 0;
}

/* Return a hash value for the SCC on the SCC stack from FIRST with SIZE.
   THIS_REF_P and REF_P are as passed to lto_output_tree for FIRST.  */

hashval_t
DFS::hash_scc (struct output_block *ob, unsigned first, unsigned size,
	       bool ref_p, bool this_ref_p)
{
  unsigned int last_classes = 0, iterations = 0;

  /* Compute hash values for the SCC members.  */
  for (unsigned i = 0; i < size; ++i)
    sccstack[first+i].hash
      = hash_tree (ob->writer_cache, NULL, sccstack[first+i].t);

  if (size == 1)
    return sccstack[first].hash;

  /* We aim to get unique hash for every tree within SCC and compute hash value
     of the whole SCC by combining all values together in a stable (entry-point
     independent) order.  This guarantees that the same SCC regions within
     different translation units will get the same hash values and therefore
     will be merged at WPA time.

     Often the hashes are already unique.  In that case we compute the SCC hash
     by combining individual hash values in an increasing order.

     If there are duplicates, we seek at least one tree with unique hash (and
     pick one with minimal hash and this property).  Then we obtain a stable
     order by DFS walk starting from this unique tree and then use the index
     within this order to make individual hash values unique.

     If there is no tree with unique hash, we iteratively propagate the hash
     values across the internal edges of SCC.  This usually quickly leads
     to unique hashes.  Consider, for example, an SCC containing two pointers
     that are identical except for the types they point to and assume that
     these types are also part of the SCC.  The propagation will add the
     points-to type information into their hash values.  */
  do
    {
      /* Sort the SCC so we can easily check for uniqueness.  */
      qsort (&sccstack[first], size, sizeof (scc_entry), scc_entry_compare);

      unsigned int classes = 1;
      int firstunique = -1;

      /* Find the tree with lowest unique hash (if it exists) and compute
	 the number of equivalence classes.  */
      if (sccstack[first].hash != sccstack[first+1].hash)
	firstunique = 0;
      for (unsigned i = 1; i < size; ++i)
	if (sccstack[first+i-1].hash != sccstack[first+i].hash)
	  {
	    classes++;
	    if (firstunique == -1
		&& (i == size - 1
		    || sccstack[first+i+1].hash != sccstack[first+i].hash))
	      firstunique = i;
	  }

      /* If we found a tree with unique hash, stop the iteration.  */
      if (firstunique != -1
	  /* Also terminate if we run out of iterations or if the number of
	     equivalence classes is no longer increasing.
	     For example a cyclic list of trees that are all equivalent will
	     never have unique entry point; we however do not build such SCCs
	     in our IL.  */
	  || classes <= last_classes || iterations > 16)
	{
          hashval_t scc_hash;

	  /* If some hashes are not unique (CLASSES != SIZE), use the DFS walk
	     starting from FIRSTUNIQUE to obtain a stable order.  */
	  if (classes != size && firstunique != -1)
	    {
	      hash_map <tree, hashval_t> map(size*2);

	      /* Store hash values into a map, so we can associate them with
		 the reordered SCC.  */
	      for (unsigned i = 0; i < size; ++i)
		map.put (sccstack[first+i].t, sccstack[first+i].hash);

	      DFS again (ob, sccstack[first+firstunique].t, ref_p, this_ref_p,
			 true);
	      gcc_assert (again.sccstack.length () == size);

	      memcpy (sccstack.address () + first,
		      again.sccstack.address (),
		      sizeof (scc_entry) * size);

	      /* Update hash values of individual members by hashing in the
		 index within the stable order.  This ensures uniqueness.
		 Also compute the SCC hash by mixing in all hash values in
		 the stable order we obtained.  */
	      sccstack[first].hash = *map.get (sccstack[first].t);
	      scc_hash = sccstack[first].hash;
	      for (unsigned i = 1; i < size; ++i)
		{
		  sccstack[first+i].hash
		    = iterative_hash_hashval_t (i,
						*map.get (sccstack[first+i].t));
		  scc_hash
		    = iterative_hash_hashval_t (scc_hash,
						sccstack[first+i].hash);
		}
	    }
	  /* If we got a unique hash value for each tree, then sort already
	     ensured entry-point independent order.  Only compute the final
	     SCC hash.

	     If we failed to find the unique entry point, we go by the same
	     route.  We will eventually introduce unwanted hash conflicts.  */
	  else
	    {
	      scc_hash = sccstack[first].hash;
	      for (unsigned i = 1; i < size; ++i)
		scc_hash
		  = iterative_hash_hashval_t (scc_hash, sccstack[first+i].hash);

	      /* We cannot 100% guarantee that the hash won't conflict so as
		 to make it impossible to find a unique hash.  This however
		 should be an extremely rare case.  ICE for now so possible
		 issues are found and evaluated.  */
	      gcc_checking_assert (classes == size);
	    }

	  /* To avoid conflicts across SCCs, iteratively hash the whole SCC
	     hash into the hash of each element.  */
	  for (unsigned i = 0; i < size; ++i)
	    sccstack[first+i].hash
	      = iterative_hash_hashval_t (sccstack[first+i].hash, scc_hash);
	  return scc_hash;
	}

      last_classes = classes;
      iterations++;

      /* We failed to identify the entry point; propagate hash values across
	 the edges.  */
      hash_map <tree, hashval_t> map(size*2);

      for (unsigned i = 0; i < size; ++i)
	map.put (sccstack[first+i].t, sccstack[first+i].hash);

      for (unsigned i = 0; i < size; i++)
	sccstack[first+i].hash
	  = hash_tree (ob->writer_cache, &map, sccstack[first+i].t);
    }
  while (true);
}

/* DFS walk EXPR and stream SCCs of tree bodies if they are not
   already in the streamer cache.  Main routine called for
   each visit of EXPR.  */

void
DFS::DFS_write_tree (struct output_block *ob, sccs *from_state,
		     tree expr, bool ref_p, bool this_ref_p)
{
  /* Handle special cases.  */
  if (expr == NULL_TREE)
    return;

  /* Do not DFS walk into indexable trees.  */
  if (this_ref_p && tree_is_indexable (expr))
    return;

  /* Check if we already streamed EXPR.  */
  if (streamer_tree_cache_lookup (ob->writer_cache, expr, NULL))
    return;

  worklist w;
  w.expr = expr;
  w.from_state = from_state;
  w.cstate = NULL;
  w.ref_p = ref_p;
  w.this_ref_p = this_ref_p;
  worklist_vec.safe_push (w);
}


/* Emit the physical representation of tree node EXPR to output block OB.
   If THIS_REF_P is true, the leaves of EXPR are emitted as references via
   lto_output_tree_ref.  REF_P is used for streaming siblings of EXPR.  */

void
lto_output_tree (struct output_block *ob, tree expr,
		 bool ref_p, bool this_ref_p)
{
  unsigned ix;
  bool existed_p;

  if (expr == NULL_TREE)
    {
      streamer_write_record_start (ob, LTO_null);
      return;
    }

  if (this_ref_p && tree_is_indexable (expr))
    {
      lto_output_tree_ref (ob, expr);
      return;
    }

  existed_p = streamer_tree_cache_lookup (ob->writer_cache, expr, &ix);
  if (existed_p)
    {
      /* If a node has already been streamed out, make sure that
	 we don't write it more than once.  Otherwise, the reader
	 will instantiate two different nodes for the same object.  */
      streamer_write_record_start (ob, LTO_tree_pickle_reference);
      streamer_write_uhwi (ob, ix);
      streamer_write_enum (ob->main_stream, LTO_tags, LTO_NUM_TAGS,
			   lto_tree_code_to_tag (TREE_CODE (expr)));
      lto_stats.num_pickle_refs_output++;
    }
  else
    {
      /* This is the first time we see EXPR, write all reachable
	 trees to OB.  */
      static bool in_dfs_walk;

      /* Protect against recursion which means disconnect between
         what tree edges we walk in the DFS walk and what edges
	 we stream out.  */
      gcc_assert (!in_dfs_walk);

      /* Start the DFS walk.  */
      /* Save ob state ... */
      /* let's see ... */
      in_dfs_walk = true;
      DFS (ob, expr, ref_p, this_ref_p, false);
      in_dfs_walk = false;

      /* Finally append a reference to the tree we were writing.
	 ???  If expr ended up as a singleton we could have
	 inlined it here and avoid outputting a reference.  */
      existed_p = streamer_tree_cache_lookup (ob->writer_cache, expr, &ix);
      gcc_assert (existed_p);
      streamer_write_record_start (ob, LTO_tree_pickle_reference);
      streamer_write_uhwi (ob, ix);
      streamer_write_enum (ob->main_stream, LTO_tags, LTO_NUM_TAGS,
			   lto_tree_code_to_tag (TREE_CODE (expr)));
      lto_stats.num_pickle_refs_output++;
    }
}


/* Output to OB a list of try/catch handlers starting with FIRST.  */

static void
output_eh_try_list (struct output_block *ob, eh_catch first)
{
  eh_catch n;

  for (n = first; n; n = n->next_catch)
    {
      streamer_write_record_start (ob, LTO_eh_catch);
      stream_write_tree (ob, n->type_list, true);
      stream_write_tree (ob, n->filter_list, true);
      stream_write_tree (ob, n->label, true);
    }

  streamer_write_record_start (ob, LTO_null);
}


/* Output EH region R in function FN to OB.  CURR_RN is the slot index
   that is being emitted in FN->EH->REGION_ARRAY.  This is used to
   detect EH region sharing.  */

static void
output_eh_region (struct output_block *ob, eh_region r)
{
  enum LTO_tags tag;

  if (r == NULL)
    {
      streamer_write_record_start (ob, LTO_null);
      return;
    }

  if (r->type == ERT_CLEANUP)
    tag = LTO_ert_cleanup;
  else if (r->type == ERT_TRY)
    tag = LTO_ert_try;
  else if (r->type == ERT_ALLOWED_EXCEPTIONS)
    tag = LTO_ert_allowed_exceptions;
  else if (r->type == ERT_MUST_NOT_THROW)
    tag = LTO_ert_must_not_throw;
  else
    gcc_unreachable ();

  streamer_write_record_start (ob, tag);
  streamer_write_hwi (ob, r->index);

  if (r->outer)
    streamer_write_hwi (ob, r->outer->index);
  else
    streamer_write_zero (ob);

  if (r->inner)
    streamer_write_hwi (ob, r->inner->index);
  else
    streamer_write_zero (ob);

  if (r->next_peer)
    streamer_write_hwi (ob, r->next_peer->index);
  else
    streamer_write_zero (ob);

  if (r->type == ERT_TRY)
    {
      output_eh_try_list (ob, r->u.eh_try.first_catch);
    }
  else if (r->type == ERT_ALLOWED_EXCEPTIONS)
    {
      stream_write_tree (ob, r->u.allowed.type_list, true);
      stream_write_tree (ob, r->u.allowed.label, true);
      streamer_write_uhwi (ob, r->u.allowed.filter);
    }
  else if (r->type == ERT_MUST_NOT_THROW)
    {
      stream_write_tree (ob, r->u.must_not_throw.failure_decl, true);
      bitpack_d bp = bitpack_create (ob->main_stream);
      stream_output_location (ob, &bp, r->u.must_not_throw.failure_loc);
      streamer_write_bitpack (&bp);
    }

  if (r->landing_pads)
    streamer_write_hwi (ob, r->landing_pads->index);
  else
    streamer_write_zero (ob);
}


/* Output landing pad LP to OB.  */

static void
output_eh_lp (struct output_block *ob, eh_landing_pad lp)
{
  if (lp == NULL)
    {
      streamer_write_record_start (ob, LTO_null);
      return;
    }

  streamer_write_record_start (ob, LTO_eh_landing_pad);
  streamer_write_hwi (ob, lp->index);
  if (lp->next_lp)
    streamer_write_hwi (ob, lp->next_lp->index);
  else
    streamer_write_zero (ob);

  if (lp->region)
    streamer_write_hwi (ob, lp->region->index);
  else
    streamer_write_zero (ob);

  stream_write_tree (ob, lp->post_landing_pad, true);
}


/* Output the existing eh_table to OB.  */

static void
output_eh_regions (struct output_block *ob, struct function *fn)
{
  if (fn->eh && fn->eh->region_tree)
    {
      unsigned i;
      eh_region eh;
      eh_landing_pad lp;
      tree ttype;

      streamer_write_record_start (ob, LTO_eh_table);

      /* Emit the index of the root of the EH region tree.  */
      streamer_write_hwi (ob, fn->eh->region_tree->index);

      /* Emit all the EH regions in the region array.  */
      streamer_write_hwi (ob, vec_safe_length (fn->eh->region_array));
      FOR_EACH_VEC_SAFE_ELT (fn->eh->region_array, i, eh)
	output_eh_region (ob, eh);

      /* Emit all landing pads.  */
      streamer_write_hwi (ob, vec_safe_length (fn->eh->lp_array));
      FOR_EACH_VEC_SAFE_ELT (fn->eh->lp_array, i, lp)
	output_eh_lp (ob, lp);

      /* Emit all the runtime type data.  */
      streamer_write_hwi (ob, vec_safe_length (fn->eh->ttype_data));
      FOR_EACH_VEC_SAFE_ELT (fn->eh->ttype_data, i, ttype)
	stream_write_tree (ob, ttype, true);

      /* Emit the table of action chains.  */
      if (targetm.arm_eabi_unwinder)
	{
	  tree t;
	  streamer_write_hwi (ob, vec_safe_length (fn->eh->ehspec_data.arm_eabi));
	  FOR_EACH_VEC_SAFE_ELT (fn->eh->ehspec_data.arm_eabi, i, t)
	    stream_write_tree (ob, t, true);
	}
      else
	{
	  uchar c;
	  streamer_write_hwi (ob, vec_safe_length (fn->eh->ehspec_data.other));
	  FOR_EACH_VEC_SAFE_ELT (fn->eh->ehspec_data.other, i, c)
	    streamer_write_char_stream (ob->main_stream, c);
	}
    }

  /* The LTO_null either terminates the record or indicates that there
     are no eh_records at all.  */
  streamer_write_record_start (ob, LTO_null);
}


/* Output all of the active ssa names to the ssa_names stream.  */

static void
output_ssa_names (struct output_block *ob, struct function *fn)
{
  unsigned int i, len;

  len = vec_safe_length (SSANAMES (fn));
  streamer_write_uhwi (ob, len);

  for (i = 1; i < len; i++)
    {
      tree ptr = (*SSANAMES (fn))[i];

      if (ptr == NULL_TREE
	  || SSA_NAME_IN_FREE_LIST (ptr)
	  || virtual_operand_p (ptr)
	  /* Simply skip unreleased SSA names.  */
	  || (! SSA_NAME_IS_DEFAULT_DEF (ptr)
	      && (! SSA_NAME_DEF_STMT (ptr)
		  || ! gimple_bb (SSA_NAME_DEF_STMT (ptr)))))
	continue;

      streamer_write_uhwi (ob, i);
      streamer_write_char_stream (ob->main_stream,
				  SSA_NAME_IS_DEFAULT_DEF (ptr));
      if (SSA_NAME_VAR (ptr))
	stream_write_tree (ob, SSA_NAME_VAR (ptr), true);
      else
	/* ???  This drops SSA_NAME_IDENTIFIER on the floor.  */
	stream_write_tree (ob, TREE_TYPE (ptr), true);
    }

  streamer_write_zero (ob);
}



/* Output the cfg.  */

static void
output_cfg (struct output_block *ob, struct function *fn)
{
  struct lto_output_stream *tmp_stream = ob->main_stream;
  basic_block bb;

  ob->main_stream = ob->cfg_stream;

  streamer_write_enum (ob->main_stream, profile_status_d, PROFILE_LAST,
		       profile_status_for_fn (fn));

  /* Output the number of the highest basic block.  */
  streamer_write_uhwi (ob, last_basic_block_for_fn (fn));

  FOR_ALL_BB_FN (bb, fn)
    {
      edge_iterator ei;
      edge e;

      streamer_write_hwi (ob, bb->index);

      /* Output the successors and the edge flags.  */
      streamer_write_uhwi (ob, EDGE_COUNT (bb->succs));
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  streamer_write_uhwi (ob, e->dest->index);
	  streamer_write_hwi (ob, e->probability);
	  streamer_write_gcov_count (ob, e->count);
	  streamer_write_uhwi (ob, e->flags);
	}
    }

  streamer_write_hwi (ob, -1);

  bb = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  while (bb->next_bb)
    {
      streamer_write_hwi (ob, bb->next_bb->index);
      bb = bb->next_bb;
    }

  streamer_write_hwi (ob, -1);

  /* ???  The cfgloop interface is tied to cfun.  */
  gcc_assert (cfun == fn);

  /* Output the number of loops.  */
  streamer_write_uhwi (ob, number_of_loops (fn));

  /* Output each loop, skipping the tree root which has number zero.  */
  for (unsigned i = 1; i < number_of_loops (fn); ++i)
    {
      struct loop *loop = get_loop (fn, i);

      /* Write the index of the loop header.  That's enough to rebuild
         the loop tree on the reader side.  Stream -1 for an unused
	 loop entry.  */
      if (!loop)
	{
	  streamer_write_hwi (ob, -1);
	  continue;
	}
      else
	streamer_write_hwi (ob, loop->header->index);

      /* Write everything copy_loop_info copies.  */
      streamer_write_enum (ob->main_stream,
			   loop_estimation, EST_LAST, loop->estimate_state);
      streamer_write_hwi (ob, loop->any_upper_bound);
      if (loop->any_upper_bound)
	streamer_write_widest_int (ob, loop->nb_iterations_upper_bound);
      streamer_write_hwi (ob, loop->any_likely_upper_bound);
      if (loop->any_likely_upper_bound)
	streamer_write_widest_int (ob, loop->nb_iterations_likely_upper_bound);
      streamer_write_hwi (ob, loop->any_estimate);
      if (loop->any_estimate)
	streamer_write_widest_int (ob, loop->nb_iterations_estimate);

      /* Write OMP SIMD related info.  */
      streamer_write_hwi (ob, loop->safelen);
      streamer_write_hwi (ob, loop->dont_vectorize);
      streamer_write_hwi (ob, loop->force_vectorize);
      stream_write_tree (ob, loop->simduid, true);
    }

  ob->main_stream = tmp_stream;
}


/* Create the header in the file using OB.  If the section type is for
   a function, set FN to the decl for that function.  */

void
produce_asm (struct output_block *ob, tree fn)
{
  enum lto_section_type section_type = ob->section_type;
  struct lto_function_header header;
  char *section_name;

  if (section_type == LTO_section_function_body)
    {
      const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fn));
      section_name = lto_get_section_name (section_type, name, NULL);
    }
  else
    section_name = lto_get_section_name (section_type, NULL, NULL);

  lto_begin_section (section_name, !flag_wpa);
  free (section_name);

  /* The entire header is stream computed here.  */
  memset (&header, 0, sizeof (struct lto_function_header));

  /* Write the header.  */
  header.major_version = LTO_major_version;
  header.minor_version = LTO_minor_version;

  if (section_type == LTO_section_function_body)
    header.cfg_size = ob->cfg_stream->total_size;
  header.main_size = ob->main_stream->total_size;
  header.string_size = ob->string_stream->total_size;
  lto_write_data (&header, sizeof header);

  /* Put all of the gimple and the string table out the asm file as a
     block of text.  */
  if (section_type == LTO_section_function_body)
    lto_write_stream (ob->cfg_stream);
  lto_write_stream (ob->main_stream);
  lto_write_stream (ob->string_stream);

  lto_end_section ();
}


/* Output the base body of struct function FN using output block OB.  */

static void
output_struct_function_base (struct output_block *ob, struct function *fn)
{
  struct bitpack_d bp;
  unsigned i;
  tree t;

  /* Output the static chain and non-local goto save area.  */
  stream_write_tree (ob, fn->static_chain_decl, true);
  stream_write_tree (ob, fn->nonlocal_goto_save_area, true);

  /* Output all the local variables in the function.  */
  streamer_write_hwi (ob, vec_safe_length (fn->local_decls));
  FOR_EACH_VEC_SAFE_ELT (fn->local_decls, i, t)
    stream_write_tree (ob, t, true);

  /* Output current IL state of the function.  */
  streamer_write_uhwi (ob, fn->curr_properties);

  /* Write all the attributes for FN.  */
  bp = bitpack_create (ob->main_stream);
  bp_pack_value (&bp, fn->is_thunk, 1);
  bp_pack_value (&bp, fn->has_local_explicit_reg_vars, 1);
  bp_pack_value (&bp, fn->returns_pcc_struct, 1);
  bp_pack_value (&bp, fn->returns_struct, 1);
  bp_pack_value (&bp, fn->can_throw_non_call_exceptions, 1);
  bp_pack_value (&bp, fn->can_delete_dead_exceptions, 1);
  bp_pack_value (&bp, fn->always_inline_functions_inlined, 1);
  bp_pack_value (&bp, fn->after_inlining, 1);
  bp_pack_value (&bp, fn->stdarg, 1);
  bp_pack_value (&bp, fn->has_nonlocal_label, 1);
  bp_pack_value (&bp, fn->has_forced_label_in_static, 1);
  bp_pack_value (&bp, fn->calls_alloca, 1);
  bp_pack_value (&bp, fn->calls_setjmp, 1);
  bp_pack_value (&bp, fn->has_force_vectorize_loops, 1);
  bp_pack_value (&bp, fn->has_simduid_loops, 1);
  bp_pack_value (&bp, fn->va_list_fpr_size, 8);
  bp_pack_value (&bp, fn->va_list_gpr_size, 8);
  bp_pack_value (&bp, fn->last_clique, sizeof (short) * 8);

  /* Output the function start and end loci.  */
  stream_output_location (ob, &bp, fn->function_start_locus);
  stream_output_location (ob, &bp, fn->function_end_locus);

  streamer_write_bitpack (&bp);
}


/* Collect all leaf BLOCKs beyond ROOT into LEAFS.  */

static void
collect_block_tree_leafs (tree root, vec<tree> &leafs)
{
  for (root = BLOCK_SUBBLOCKS (root); root; root = BLOCK_CHAIN (root))
    if (! BLOCK_SUBBLOCKS (root))
      leafs.safe_push (root);
    else
      collect_block_tree_leafs (BLOCK_SUBBLOCKS (root), leafs);
}

/* Output the body of function NODE->DECL.  */

static void
output_function (struct cgraph_node *node)
{
  tree function;
  struct function *fn;
  basic_block bb;
  struct output_block *ob;

  function = node->decl;
  fn = DECL_STRUCT_FUNCTION (function);
  ob = create_output_block (LTO_section_function_body);

  clear_line_info (ob);
  ob->symbol = node;

  gcc_assert (current_function_decl == NULL_TREE && cfun == NULL);

  /* Set current_function_decl and cfun.  */
  push_cfun (fn);

  /* Make string 0 be a NULL string.  */
  streamer_write_char_stream (ob->string_stream, 0);

  streamer_write_record_start (ob, LTO_function);

  /* Output decls for parameters and args.  */
  stream_write_tree (ob, DECL_RESULT (function), true);
  streamer_write_chain (ob, DECL_ARGUMENTS (function), true);

  /* Output DECL_INITIAL for the function, which contains the tree of
     lexical scopes.  */
  stream_write_tree (ob, DECL_INITIAL (function), true);
  /* As we do not recurse into BLOCK_SUBBLOCKS but only BLOCK_SUPERCONTEXT
     collect block tree leafs and stream those.  */
  auto_vec<tree> block_tree_leafs;
  if (DECL_INITIAL (function))
    collect_block_tree_leafs (DECL_INITIAL (function), block_tree_leafs);
  streamer_write_uhwi (ob, block_tree_leafs.length ());
  for (unsigned i = 0; i < block_tree_leafs.length (); ++i)
    stream_write_tree (ob, block_tree_leafs[i], true);

  /* We also stream abstract functions where we stream only stuff needed for
     debug info.  */
  if (gimple_has_body_p (function))
    {
      streamer_write_uhwi (ob, 1);
      output_struct_function_base (ob, fn);

      /* Output all the SSA names used in the function.  */
      output_ssa_names (ob, fn);

      /* Output any exception handling regions.  */
      output_eh_regions (ob, fn);


      /* We will renumber the statements.  The code that does this uses
	 the same ordering that we use for serializing them so we can use
	 the same code on the other end and not have to write out the
	 statement numbers.  We do not assign UIDs to PHIs here because
	 virtual PHIs get re-computed on-the-fly which would make numbers
	 inconsistent.  */
      set_gimple_stmt_max_uid (cfun, 0);
      FOR_ALL_BB_FN (bb, cfun)
	{
	  for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	       gsi_next (&gsi))
	    {
	      gphi *stmt = gsi.phi ();

	      /* Virtual PHIs are not going to be streamed.  */
	      if (!virtual_operand_p (gimple_phi_result (stmt)))
	        gimple_set_uid (stmt, inc_gimple_stmt_max_uid (cfun));
	    }
	  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	       gsi_next (&gsi))
	    {
	      gimple *stmt = gsi_stmt (gsi);
	      gimple_set_uid (stmt, inc_gimple_stmt_max_uid (cfun));
	    }
	}
      /* To avoid keeping duplicate gimple IDs in the statements, renumber
	 virtual phis now.  */
      FOR_ALL_BB_FN (bb, cfun)
	{
	  for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	       gsi_next (&gsi))
	    {
	      gphi *stmt = gsi.phi ();
	      if (virtual_operand_p (gimple_phi_result (stmt)))
	        gimple_set_uid (stmt, inc_gimple_stmt_max_uid (cfun));
	    }
	}

      /* Output the code for the function.  */
      FOR_ALL_BB_FN (bb, fn)
	output_bb (ob, bb, fn);

      /* The terminator for this function.  */
      streamer_write_record_start (ob, LTO_null);

      output_cfg (ob, fn);

      pop_cfun ();
   }
  else
    streamer_write_uhwi (ob, 0);

  /* Create a section to hold the pickled output of this function.   */
  produce_asm (ob, function);

  destroy_output_block (ob);
}

/* Output the body of function NODE->DECL.  */

static void
output_constructor (struct varpool_node *node)
{
  tree var = node->decl;
  struct output_block *ob;

  ob = create_output_block (LTO_section_function_body);

  clear_line_info (ob);
  ob->symbol = node;

  /* Make string 0 be a NULL string.  */
  streamer_write_char_stream (ob->string_stream, 0);

  /* Output DECL_INITIAL for the function, which contains the tree of
     lexical scopes.  */
  stream_write_tree (ob, DECL_INITIAL (var), true);

  /* Create a section to hold the pickled output of this function.   */
  produce_asm (ob, var);

  destroy_output_block (ob);
}


/* Emit toplevel asms.  */

void
lto_output_toplevel_asms (void)
{
  struct output_block *ob;
  struct asm_node *can;
  char *section_name;
  struct lto_simple_header_with_strings header;

  if (!symtab->first_asm_symbol ())
    return;

  ob = create_output_block (LTO_section_asm);

  /* Make string 0 be a NULL string.  */
  streamer_write_char_stream (ob->string_stream, 0);

  for (can = symtab->first_asm_symbol (); can; can = can->next)
    {
      streamer_write_string_cst (ob, ob->main_stream, can->asm_str);
      streamer_write_hwi (ob, can->order);
    }

  streamer_write_string_cst (ob, ob->main_stream, NULL_TREE);

  section_name = lto_get_section_name (LTO_section_asm, NULL, NULL);
  lto_begin_section (section_name, !flag_wpa);
  free (section_name);

  /* The entire header stream is computed here.  */
  memset (&header, 0, sizeof (header));

  /* Write the header.  */
  header.major_version = LTO_major_version;
  header.minor_version = LTO_minor_version;

  header.main_size = ob->main_stream->total_size;
  header.string_size = ob->string_stream->total_size;
  lto_write_data (&header, sizeof header);

  /* Put all of the gimple and the string table out the asm file as a
     block of text.  */
  lto_write_stream (ob->main_stream);
  lto_write_stream (ob->string_stream);

  lto_end_section ();

  destroy_output_block (ob);
}


/* Copy the function body or variable constructor of NODE without deserializing. */

static void
copy_function_or_variable (struct symtab_node *node)
{
  tree function = node->decl;
  struct lto_file_decl_data *file_data = node->lto_file_data;
  const char *data;
  size_t len;
  const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (function));
  char *section_name =
    lto_get_section_name (LTO_section_function_body, name, NULL);
  size_t i, j;
  struct lto_in_decl_state *in_state;
  struct lto_out_decl_state *out_state = lto_get_out_decl_state ();

  lto_begin_section (section_name, false);
  free (section_name);

  /* We may have renamed the declaration, e.g., a static function.  */
  name = lto_get_decl_name_mapping (file_data, name);

  data = lto_get_raw_section_data (file_data, LTO_section_function_body,
                                   name, &len);
  gcc_assert (data);

  /* Do a bit copy of the function body.  */
  lto_write_raw_data (data, len);

  /* Copy decls. */
  in_state =
    lto_get_function_in_decl_state (node->lto_file_data, function);
  out_state->compressed = in_state->compressed;
  gcc_assert (in_state);

  for (i = 0; i < LTO_N_DECL_STREAMS; i++)
    {
      size_t n = vec_safe_length (in_state->streams[i]);
      vec<tree, va_gc> *trees = in_state->streams[i];
      struct lto_tree_ref_encoder *encoder = &(out_state->streams[i]);

      /* The out state must have the same indices and the in state.
	 So just copy the vector.  All the encoders in the in state
	 must be empty where we reach here. */
      gcc_assert (lto_tree_ref_encoder_size (encoder) == 0);
      encoder->trees.reserve_exact (n);
      for (j = 0; j < n; j++)
	encoder->trees.safe_push ((*trees)[j]);
    }

  lto_free_raw_section_data (file_data, LTO_section_function_body, name,
			     data, len);
  lto_end_section ();
}

/* Wrap symbol references in *TP inside a type-preserving MEM_REF.  */

static tree
wrap_refs (tree *tp, int *ws, void *)
{
  tree t = *tp;
  if (handled_component_p (t)
      && TREE_CODE (TREE_OPERAND (t, 0)) == VAR_DECL
      && TREE_PUBLIC (TREE_OPERAND (t, 0)))
    {
      tree decl = TREE_OPERAND (t, 0);
      tree ptrtype = build_pointer_type (TREE_TYPE (decl));
      TREE_OPERAND (t, 0) = build2 (MEM_REF, TREE_TYPE (decl),
				    build1 (ADDR_EXPR, ptrtype, decl),
				    build_int_cst (ptrtype, 0));
      TREE_THIS_VOLATILE (TREE_OPERAND (t, 0)) = TREE_THIS_VOLATILE (decl);
      *ws = 0;
    }
  else if (TREE_CODE (t) == CONSTRUCTOR)
    ;
  else if (!EXPR_P (t))
    *ws = 0;
  return NULL_TREE;
}

/* Main entry point from the pass manager.  */

void
lto_output (void)
{
  struct lto_out_decl_state *decl_state;
  bitmap output = NULL;
  int i, n_nodes;
  lto_symtab_encoder_t encoder = lto_get_out_decl_state ()->symtab_node_encoder;

  if (flag_checking)
    output = lto_bitmap_alloc ();

  /* Initialize the streamer.  */
  lto_streamer_init ();

  n_nodes = lto_symtab_encoder_size (encoder);
  /* Process only the functions with bodies.  */
  for (i = 0; i < n_nodes; i++)
    {
      symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
      if (cgraph_node *node = dyn_cast <cgraph_node *> (snode))
	{
	  if (lto_symtab_encoder_encode_body_p (encoder, node)
	      && !node->alias
	      && (!node->thunk.thunk_p || !node->thunk.add_pointer_bounds_args))
	    {
	      if (flag_checking)
		{
		  gcc_assert (!bitmap_bit_p (output, DECL_UID (node->decl)));
		  bitmap_set_bit (output, DECL_UID (node->decl));
		}
	      decl_state = lto_new_out_decl_state ();
	      lto_push_out_decl_state (decl_state);
	      if (gimple_has_body_p (node->decl) || !flag_wpa
		  /* Thunks have no body but they may be synthetized
		     at WPA time.  */
		  || DECL_ARGUMENTS (node->decl))
		output_function (node);
	      else
		copy_function_or_variable (node);
	      gcc_assert (lto_get_out_decl_state () == decl_state);
	      lto_pop_out_decl_state ();
	      lto_record_function_out_decl_state (node->decl, decl_state);
	    }
	}
      else if (varpool_node *node = dyn_cast <varpool_node *> (snode))
	{
	  /* Wrap symbol references inside the ctor in a type
	     preserving MEM_REF.  */
	  tree ctor = DECL_INITIAL (node->decl);
	  if (ctor && !in_lto_p)
	    walk_tree (&ctor, wrap_refs, NULL, NULL);
	  if (get_symbol_initial_value (encoder, node->decl) == error_mark_node
	      && lto_symtab_encoder_encode_initializer_p (encoder, node)
	      && !node->alias)
	    {
	      timevar_push (TV_IPA_LTO_CTORS_OUT);
	      if (flag_checking)
		{
		  gcc_assert (!bitmap_bit_p (output, DECL_UID (node->decl)));
		  bitmap_set_bit (output, DECL_UID (node->decl));
		}
	      decl_state = lto_new_out_decl_state ();
	      lto_push_out_decl_state (decl_state);
	      if (DECL_INITIAL (node->decl) != error_mark_node
		  || !flag_wpa)
		output_constructor (node);
	      else
		copy_function_or_variable (node);
	      gcc_assert (lto_get_out_decl_state () == decl_state);
	      lto_pop_out_decl_state ();
	      lto_record_function_out_decl_state (node->decl, decl_state);
	      timevar_pop (TV_IPA_LTO_CTORS_OUT);
	    }
	}
    }

  /* Emit the callgraph after emitting function bodies.  This needs to
     be done now to make sure that all the statements in every function
     have been renumbered so that edges can be associated with call
     statements using the statement UIDs.  */
  output_symtab ();

  output_offload_tables ();

#if CHECKING_P
  lto_bitmap_free (output);
#endif
}

/* Write each node in encoded by ENCODER to OB, as well as those reachable
   from it and required for correct representation of its semantics.
   Each node in ENCODER must be a global declaration or a type.  A node
   is written only once, even if it appears multiple times in the
   vector.  Certain transitively-reachable nodes, such as those
   representing expressions, may be duplicated, but such nodes
   must not appear in ENCODER itself.  */

static void
write_global_stream (struct output_block *ob,
		     struct lto_tree_ref_encoder *encoder)
{
  tree t;
  size_t index;
  const size_t size = lto_tree_ref_encoder_size (encoder);

  for (index = 0; index < size; index++)
    {
      t = lto_tree_ref_encoder_get_tree (encoder, index);
      if (!streamer_tree_cache_lookup (ob->writer_cache, t, NULL))
	stream_write_tree (ob, t, false);
    }
}


/* Write a sequence of indices into the globals vector corresponding
   to the trees in ENCODER.  These are used by the reader to map the
   indices used to refer to global entities within function bodies to
   their referents.  */

static void
write_global_references (struct output_block *ob,
 			 struct lto_tree_ref_encoder *encoder)
{
  tree t;
  uint32_t index;
  const uint32_t size = lto_tree_ref_encoder_size (encoder);

  /* Write size and slot indexes as 32-bit unsigned numbers. */
  uint32_t *data = XNEWVEC (uint32_t, size + 1);
  data[0] = size;

  for (index = 0; index < size; index++)
    {
      unsigned slot_num;

      t = lto_tree_ref_encoder_get_tree (encoder, index);
      streamer_tree_cache_lookup (ob->writer_cache, t, &slot_num);
      gcc_assert (slot_num != (unsigned)-1);
      data[index + 1] = slot_num;
    }

  lto_write_data (data, sizeof (int32_t) * (size + 1));
  free (data);
}


/* Write all the streams in an lto_out_decl_state STATE using
   output block OB and output stream OUT_STREAM.  */

void
lto_output_decl_state_streams (struct output_block *ob,
			       struct lto_out_decl_state *state)
{
  int i;

  for (i = 0;  i < LTO_N_DECL_STREAMS; i++)
    write_global_stream (ob, &state->streams[i]);
}


/* Write all the references in an lto_out_decl_state STATE using
   output block OB and output stream OUT_STREAM.  */

void
lto_output_decl_state_refs (struct output_block *ob,
			    struct lto_out_decl_state *state)
{
  unsigned i;
  unsigned ref;
  tree decl;

  /* Write reference to FUNCTION_DECL.  If there is not function,
     write reference to void_type_node. */
  decl = (state->fn_decl) ? state->fn_decl : void_type_node;
  streamer_tree_cache_lookup (ob->writer_cache, decl, &ref);
  gcc_assert (ref != (unsigned)-1);
  ref = ref * 2 + (state->compressed ? 1 : 0);
  lto_write_data (&ref, sizeof (uint32_t));

  for (i = 0;  i < LTO_N_DECL_STREAMS; i++)
    write_global_references (ob, &state->streams[i]);
}


/* Return the written size of STATE. */

static size_t
lto_out_decl_state_written_size (struct lto_out_decl_state *state)
{
  int i;
  size_t size;

  size = sizeof (int32_t);	/* fn_ref. */
  for (i = 0; i < LTO_N_DECL_STREAMS; i++)
    {
      size += sizeof (int32_t); /* vector size. */
      size += (lto_tree_ref_encoder_size (&state->streams[i])
	       * sizeof (int32_t));
    }
  return size;
}


/* Write symbol T into STREAM in CACHE. SEEN specifies symbols we wrote
   so far.  */

static void
write_symbol (struct streamer_tree_cache_d *cache,
	      tree t, hash_set<const char *> *seen, bool alias)
{
  const char *name;
  enum gcc_plugin_symbol_kind kind;
  enum gcc_plugin_symbol_visibility visibility = GCCPV_DEFAULT;
  unsigned slot_num;
  uint64_t size;
  const char *comdat;
  unsigned char c;

  /* None of the following kinds of symbols are needed in the
     symbol table.  */
  if (!TREE_PUBLIC (t)
      || is_builtin_fn (t)
      || DECL_ABSTRACT_P (t)
      || (VAR_P (t) && DECL_HARD_REGISTER (t)))
    return;

  gcc_assert (VAR_OR_FUNCTION_DECL_P (t));

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (t));

  /* This behaves like assemble_name_raw in varasm.c, performing the
     same name manipulations that ASM_OUTPUT_LABELREF does. */
  name = IDENTIFIER_POINTER ((*targetm.asm_out.mangle_assembler_name) (name));

  if (seen->add (name))
    return;

  streamer_tree_cache_lookup (cache, t, &slot_num);
  gcc_assert (slot_num != (unsigned)-1);

  if (DECL_EXTERNAL (t))
    {
      if (DECL_WEAK (t))
	kind = GCCPK_WEAKUNDEF;
      else
	kind = GCCPK_UNDEF;
    }
  else
    {
      if (DECL_WEAK (t))
	kind = GCCPK_WEAKDEF;
      else if (DECL_COMMON (t))
	kind = GCCPK_COMMON;
      else
	kind = GCCPK_DEF;

      /* When something is defined, it should have node attached.  */
      gcc_assert (alias || !VAR_P (t) || varpool_node::get (t)->definition);
      gcc_assert (alias || TREE_CODE (t) != FUNCTION_DECL
		  || (cgraph_node::get (t)
		      && cgraph_node::get (t)->definition));
    }

  /* Imitate what default_elf_asm_output_external do.
     When symbol is external, we need to output it with DEFAULT visibility
     when compiling with -fvisibility=default, while with HIDDEN visibility
     when symbol has attribute (visibility("hidden")) specified.
     targetm.binds_local_p check DECL_VISIBILITY_SPECIFIED and gets this
     right. */

  if (DECL_EXTERNAL (t)
      && !targetm.binds_local_p (t))
    visibility = GCCPV_DEFAULT;
  else
    switch (DECL_VISIBILITY (t))
      {
      case VISIBILITY_DEFAULT:
	visibility = GCCPV_DEFAULT;
	break;
      case VISIBILITY_PROTECTED:
	visibility = GCCPV_PROTECTED;
	break;
      case VISIBILITY_HIDDEN:
	visibility = GCCPV_HIDDEN;
	break;
      case VISIBILITY_INTERNAL:
	visibility = GCCPV_INTERNAL;
	break;
      }

  if (kind == GCCPK_COMMON
      && DECL_SIZE_UNIT (t)
      && TREE_CODE (DECL_SIZE_UNIT (t)) == INTEGER_CST)
    size = TREE_INT_CST_LOW (DECL_SIZE_UNIT (t));
  else
    size = 0;

  if (DECL_ONE_ONLY (t))
    comdat = IDENTIFIER_POINTER (decl_comdat_group_id (t));
  else
    comdat = "";

  lto_write_data (name, strlen (name) + 1);
  lto_write_data (comdat, strlen (comdat) + 1);
  c = (unsigned char) kind;
  lto_write_data (&c, 1);
  c = (unsigned char) visibility;
  lto_write_data (&c, 1);
  lto_write_data (&size, 8);
  lto_write_data (&slot_num, 4);
}

/* Return true if NODE should appear in the plugin symbol table.  */

bool
output_symbol_p (symtab_node *node)
{
  struct cgraph_node *cnode;
  if (!node->real_symbol_p ())
    return false;
  /* We keep external functions in symtab for sake of inlining
     and devirtualization.  We do not want to see them in symbol table as
     references unless they are really used.  */
  cnode = dyn_cast <cgraph_node *> (node);
  if (cnode && (!node->definition || DECL_EXTERNAL (cnode->decl))
      && cnode->callers)
    return true;

 /* Ignore all references from external vars initializers - they are not really
    part of the compilation unit until they are used by folding.  Some symbols,
    like references to external construction vtables can not be referred to at all.
    We decide this at can_refer_decl_in_current_unit_p.  */
 if (!node->definition || DECL_EXTERNAL (node->decl))
    {
      int i;
      struct ipa_ref *ref;
      for (i = 0; node->iterate_referring (i, ref); i++)
	{
	  if (ref->use == IPA_REF_ALIAS)
	    continue;
          if (is_a <cgraph_node *> (ref->referring))
	    return true;
	  if (!DECL_EXTERNAL (ref->referring->decl))
	    return true;
	}
      return false;
    }
  return true;
}


/* Write an IL symbol table to OB.
   SET and VSET are cgraph/varpool node sets we are outputting.  */

static void
produce_symtab (struct output_block *ob)
{
  struct streamer_tree_cache_d *cache = ob->writer_cache;
  char *section_name = lto_get_section_name (LTO_section_symtab, NULL, NULL);
  lto_symtab_encoder_t encoder = ob->decl_state->symtab_node_encoder;
  lto_symtab_encoder_iterator lsei;

  lto_begin_section (section_name, false);
  free (section_name);

  hash_set<const char *> seen;

  /* Write the symbol table.
     First write everything defined and then all declarations.
     This is necessary to handle cases where we have duplicated symbols.  */
  for (lsei = lsei_start (encoder);
       !lsei_end_p (lsei); lsei_next (&lsei))
    {
      symtab_node *node = lsei_node (lsei);

      if (!output_symbol_p (node) || DECL_EXTERNAL (node->decl))
	continue;
      write_symbol (cache, node->decl, &seen, false);
    }
  for (lsei = lsei_start (encoder);
       !lsei_end_p (lsei); lsei_next (&lsei))
    {
      symtab_node *node = lsei_node (lsei);

      if (!output_symbol_p (node) || !DECL_EXTERNAL (node->decl))
	continue;
      write_symbol (cache, node->decl, &seen, false);
    }

  lto_end_section ();
}


/* Init the streamer_mode_table for output, where we collect info on what
   machine_mode values have been streamed.  */
void
lto_output_init_mode_table (void)
{
  memset (streamer_mode_table, '\0', MAX_MACHINE_MODE);
}


/* Write the mode table.  */
static void
lto_write_mode_table (void)
{
  struct output_block *ob;
  ob = create_output_block (LTO_section_mode_table);
  bitpack_d bp = bitpack_create (ob->main_stream);

  /* Ensure that for GET_MODE_INNER (m) != m we have
     also the inner mode marked.  */
  for (int i = 0; i < (int) MAX_MACHINE_MODE; i++)
    if (streamer_mode_table[i])
      {
	machine_mode m = (machine_mode) i;
	if (GET_MODE_INNER (m) != m)
	  streamer_mode_table[(int) GET_MODE_INNER (m)] = 1;
      }
  /* First stream modes that have GET_MODE_INNER (m) == m,
     so that we can refer to them afterwards.  */
  for (int pass = 0; pass < 2; pass++)
    for (int i = 0; i < (int) MAX_MACHINE_MODE; i++)
      if (streamer_mode_table[i] && i != (int) VOIDmode && i != (int) BLKmode)
	{
	  machine_mode m = (machine_mode) i;
	  if ((GET_MODE_INNER (m) == m) ^ (pass == 0))
	    continue;
	  bp_pack_value (&bp, m, 8);
	  bp_pack_enum (&bp, mode_class, MAX_MODE_CLASS, GET_MODE_CLASS (m));
	  bp_pack_value (&bp, GET_MODE_SIZE (m), 8);
	  bp_pack_value (&bp, GET_MODE_PRECISION (m), 16);
	  bp_pack_value (&bp, GET_MODE_INNER (m), 8);
	  bp_pack_value (&bp, GET_MODE_NUNITS (m), 8);
	  switch (GET_MODE_CLASS (m))
	    {
	    case MODE_FRACT:
	    case MODE_UFRACT:
	    case MODE_ACCUM:
	    case MODE_UACCUM:
	      bp_pack_value (&bp, GET_MODE_IBIT (m), 8);
	      bp_pack_value (&bp, GET_MODE_FBIT (m), 8);
	      break;
	    case MODE_FLOAT:
	    case MODE_DECIMAL_FLOAT:
	      bp_pack_string (ob, &bp, REAL_MODE_FORMAT (m)->name, true);
	      break;
	    default:
	      break;
	    }
	  bp_pack_string (ob, &bp, GET_MODE_NAME (m), true);
	}
  bp_pack_value (&bp, VOIDmode, 8);

  streamer_write_bitpack (&bp);

  char *section_name
    = lto_get_section_name (LTO_section_mode_table, NULL, NULL);
  lto_begin_section (section_name, !flag_wpa);
  free (section_name);

  /* The entire header stream is computed here.  */
  struct lto_simple_header_with_strings header;
  memset (&header, 0, sizeof (header));

  /* Write the header.  */
  header.major_version = LTO_major_version;
  header.minor_version = LTO_minor_version;

  header.main_size = ob->main_stream->total_size;
  header.string_size = ob->string_stream->total_size;
  lto_write_data (&header, sizeof header);

  /* Put all of the gimple and the string table out the asm file as a
     block of text.  */
  lto_write_stream (ob->main_stream);
  lto_write_stream (ob->string_stream);

  lto_end_section ();
  destroy_output_block (ob);
}


/* This pass is run after all of the functions are serialized and all
   of the IPA passes have written their serialized forms.  This pass
   causes the vector of all of the global decls and types used from
   this file to be written in to a section that can then be read in to
   recover these on other side.  */

void
produce_asm_for_decls (void)
{
  struct lto_out_decl_state *out_state;
  struct lto_out_decl_state *fn_out_state;
  struct lto_decl_header header;
  char *section_name;
  struct output_block *ob;
  unsigned idx, num_fns;
  size_t decl_state_size;
  int32_t num_decl_states;

  ob = create_output_block (LTO_section_decls);

  memset (&header, 0, sizeof (struct lto_decl_header));

  section_name = lto_get_section_name (LTO_section_decls, NULL, NULL);
  lto_begin_section (section_name, !flag_wpa);
  free (section_name);

  /* Make string 0 be a NULL string.  */
  streamer_write_char_stream (ob->string_stream, 0);

  gcc_assert (!alias_pairs);

  /* Get rid of the global decl state hash tables to save some memory.  */
  out_state = lto_get_out_decl_state ();
  for (int i = 0; i < LTO_N_DECL_STREAMS; i++)
    if (out_state->streams[i].tree_hash_table)
      {
	delete out_state->streams[i].tree_hash_table;
	out_state->streams[i].tree_hash_table = NULL;
      }

  /* Write the global symbols.  */
  lto_output_decl_state_streams (ob, out_state);
  num_fns = lto_function_decl_states.length ();
  for (idx = 0; idx < num_fns; idx++)
    {
      fn_out_state =
	lto_function_decl_states[idx];
      lto_output_decl_state_streams (ob, fn_out_state);
    }

  header.major_version = LTO_major_version;
  header.minor_version = LTO_minor_version;

  /* Currently not used.  This field would allow us to preallocate
     the globals vector, so that it need not be resized as it is extended.  */
  header.num_nodes = -1;

  /* Compute the total size of all decl out states. */
  decl_state_size = sizeof (int32_t);
  decl_state_size += lto_out_decl_state_written_size (out_state);
  for (idx = 0; idx < num_fns; idx++)
    {
      fn_out_state =
	lto_function_decl_states[idx];
      decl_state_size += lto_out_decl_state_written_size (fn_out_state);
    }
  header.decl_state_size = decl_state_size;

  header.main_size = ob->main_stream->total_size;
  header.string_size = ob->string_stream->total_size;

  lto_write_data (&header, sizeof header);

  /* Write the main out-decl state, followed by out-decl states of
     functions. */
  num_decl_states = num_fns + 1;
  lto_write_data (&num_decl_states, sizeof (num_decl_states));
  lto_output_decl_state_refs (ob, out_state);
  for (idx = 0; idx < num_fns; idx++)
    {
      fn_out_state = lto_function_decl_states[idx];
      lto_output_decl_state_refs (ob, fn_out_state);
    }

  lto_write_stream (ob->main_stream);
  lto_write_stream (ob->string_stream);

  lto_end_section ();

  /* Write the symbol table.  It is used by linker to determine dependencies
     and thus we can skip it for WPA.  */
  if (!flag_wpa)
    produce_symtab (ob);

  /* Write command line opts.  */
  lto_write_options ();

  /* Deallocate memory and clean up.  */
  for (idx = 0; idx < num_fns; idx++)
    {
      fn_out_state =
	lto_function_decl_states[idx];
      lto_delete_out_decl_state (fn_out_state);
    }
  lto_symtab_encoder_delete (ob->decl_state->symtab_node_encoder);
  lto_function_decl_states.release ();
  destroy_output_block (ob);
  if (lto_stream_offload_p)
    lto_write_mode_table ();
}
