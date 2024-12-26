/* Write the GIMPLE representation to a file stream.

   Copyright (C) 2009-2025 Free Software Foundation, Inc.
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
#include "debug.h"
#include "omp-offload.h"
#include "print-tree.h"
#include "tree-dfa.h"
#include "file-prefix-map.h" /* remap_debug_filename()  */
#include "output.h"
#include "ipa-utils.h"
#include "toplev.h"


static void lto_write_tree (struct output_block*, tree, bool);

/* Clear the line info stored in DATA_IN.  */

static void
clear_line_info (struct output_block *ob)
{
  ob->current_file = NULL;
  ob->current_line = 0;
  ob->current_col = 0;
  ob->current_sysp = false;
  ob->reset_locus = true;
  ob->emit_pwd = true;
  /* Initialize to something that will never appear as block,
     so that the first location with block in a function etc.
     always streams a change_block bit and the first block.  */
  ob->current_block = void_node;
  ob->current_discr = UINT_MAX;
}


/* Create the output block and return it.  SECTION_TYPE is
   LTO_section_function_body or LTO_static_initializer.  */

struct output_block *
create_output_block (enum lto_section_type section_type)
{
  struct output_block *ob = XCNEW (struct output_block);
  if (streamer_dump_file)
    fprintf (streamer_dump_file, "Creating output block for %s\n",
	     lto_section_name[section_type]);

  ob->section_type = section_type;
  ob->decl_state = lto_get_out_decl_state ();
  /* Only global decl stream in non-wpa will ever be considered by tree
     merging.  */
  if (!flag_wpa && section_type == LTO_section_decls)
    ob->local_trees = new (hash_set <tree>);
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
  delete ob->local_trees;

  free (ob->main_stream);
  free (ob->string_stream);
  if (section_type == LTO_section_function_body)
    free (ob->cfg_stream);

  streamer_tree_cache_delete (ob->writer_cache);
  obstack_free (&ob->obstack, NULL);

  free (ob);
}


/* Wrapper around variably_modified_type_p avoiding type modification
   during WPA streaming.  */

static bool
lto_variably_modified_type_p (tree type)
{
  return (in_lto_p
	  ? TYPE_LANG_FLAG_0 (TYPE_MAIN_VARIANT (type))
	  : variably_modified_type_p (type, NULL_TREE));
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
    return lto_variably_modified_type_p (TREE_TYPE (DECL_CONTEXT (t)));
  /* IMPORTED_DECL is put into BLOCK and thus it never can be shared.
     We should no longer need to stream it.  */
  else if (TREE_CODE (t) == IMPORTED_DECL)
    gcc_unreachable ();
  else if (TREE_CODE (t) == LABEL_DECL)
    return FORCED_LABEL (t) || DECL_NONLOCAL (t);
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
	   && lto_variably_modified_type_p (t))
    return false;
  else if (TREE_CODE (t) == FIELD_DECL
	   && lto_variably_modified_type_p (DECL_CONTEXT (t)))
    return false;
  else
    return (IS_TYPE_OR_DECL_P (t) || TREE_CODE (t) == SSA_NAME);
}


/* Output info about new location into bitpack BP.
   After outputting bitpack, lto_output_location_data has
   to be done to output actual data.  */

static void
lto_output_location_1 (struct output_block *ob, struct bitpack_d *bp,
		       location_t orig_loc, bool block_p)
{
  location_t loc = LOCATION_LOCUS (orig_loc);

  if (loc >= RESERVED_LOCATION_COUNT)
    {
      expanded_location xloc = expand_location (loc);
      unsigned discr = get_discriminator_from_loc (orig_loc);

      if (ob->reset_locus)
	{
	  if (xloc.file == NULL)
	    ob->current_file = "";
	  if (xloc.line == 0)
	    ob->current_line = 1;
	  if (xloc.column == 0)
	    ob->current_col = 1;
	  ob->reset_locus = false;
	}

      /* As RESERVED_LOCATION_COUNT is 2, we can use the spare value of
	 3 without wasting additional bits to signalize file change.
	 If RESERVED_LOCATION_COUNT changes, reconsider this.  */
      gcc_checking_assert (RESERVED_LOCATION_COUNT == 2);
      bp_pack_int_in_range (bp, 0, RESERVED_LOCATION_COUNT + 1,
			    RESERVED_LOCATION_COUNT
			    + (ob->current_file != xloc.file));

      bp_pack_value (bp, ob->current_line != xloc.line, 1);
      bp_pack_value (bp, ob->current_col != xloc.column, 1);
      bp_pack_value (bp, ob->current_discr != discr, 1);

      if (ob->current_file != xloc.file)
	{
	  bool stream_pwd = false;
	  const char *remapped = remap_debug_filename (xloc.file);
	  if (ob->emit_pwd && remapped && !IS_ABSOLUTE_PATH (remapped))
	    {
	      stream_pwd = true;
	      ob->emit_pwd = false;
	    }
	  bp_pack_value (bp, stream_pwd, 1);
	  if (stream_pwd)
	    bp_pack_string (ob, bp, get_src_pwd (), true);
	  bp_pack_string (ob, bp, remapped, true);
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

      if (ob->current_discr != discr)
	bp_pack_var_len_unsigned (bp, discr);
      ob->current_discr = discr;
    }
  else
    bp_pack_int_in_range (bp, 0, RESERVED_LOCATION_COUNT + 1, loc);

  if (block_p)
    {
      tree block = LOCATION_BLOCK (orig_loc);
      bp_pack_value (bp, ob->current_block != block, 1);
      streamer_write_bitpack (bp);
      if (ob->current_block != block)
	lto_output_tree (ob, block, true, true);
      ob->current_block = block;
    }
}

/* Output info about new location into bitpack BP.
   After outputting bitpack, lto_output_location_data has
   to be done to output actual data.  */

void
lto_output_location (struct output_block *ob, struct bitpack_d *bp,
		     location_t loc)
{
  lto_output_location_1 (ob, bp, loc, false);
}

/* Output info about new location into bitpack BP.
   After outputting bitpack, lto_output_location_data has
   to be done to output actual data.  Like lto_output_location, but
   additionally output LOCATION_BLOCK info too and write the BP bitpack.  */

void
lto_output_location_and_block (struct output_block *ob, struct bitpack_d *bp,
			       location_t loc)
{
  lto_output_location_1 (ob, bp, loc, true);
}


/* Lookup NAME in ENCODER.  If NAME is not found, create a new entry in
   ENCODER for NAME with the next available index of ENCODER,  then
   print the index to OBS.
   Return the index.  */


static unsigned
lto_get_index (struct lto_tree_ref_encoder *encoder, tree t)
{
  bool existed_p;

  unsigned int &index
    = encoder->tree_hash_table->get_or_insert (t, &existed_p);
  if (!existed_p)
    {
      index = encoder->trees.length ();
      if (streamer_dump_file)
	{
	  print_node_brief (streamer_dump_file, "     Encoding indexable ",
			    t, 4);
	  fprintf (streamer_dump_file, "  as %i \n", index);
	}
      encoder->trees.safe_push (t);
    }

  return index;
}


/* If EXPR is an indexable tree node, output a reference to it to
   output block OB.  Otherwise, output the physical representation of
   EXPR to OB.  */

static void
lto_indexable_tree_ref (struct output_block *ob, tree expr,
			enum LTO_tags *tag, unsigned *index)
{
  gcc_checking_assert (tree_is_indexable (expr));

  if (TREE_CODE (expr) == SSA_NAME)
    {
      *tag = LTO_ssa_name_ref;
      *index = SSA_NAME_VERSION (expr);
    }
  else
    {
      *tag = LTO_global_stream_ref;
      *index = lto_get_index (&ob->decl_state->streams[LTO_DECL_STREAM], expr);
    }
}


/* Output a static or extern var DECL to OBS.  */

void
lto_output_var_decl_ref (struct lto_out_decl_state *decl_state,
			 struct lto_output_stream * obs, tree decl)
{
  gcc_checking_assert (VAR_P (decl));
  streamer_write_uhwi_stream
     (obs, lto_get_index (&decl_state->streams[LTO_DECL_STREAM],
			  decl));
}


/* Output a static or extern var DECL to OBS.  */

void
lto_output_fn_decl_ref (struct lto_out_decl_state *decl_state,
			struct lto_output_stream * obs, tree decl)
{
  gcc_checking_assert (TREE_CODE (decl) == FUNCTION_DECL);
  streamer_write_uhwi_stream
     (obs, lto_get_index (&decl_state->streams[LTO_DECL_STREAM], decl));
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


/* Output reference to tree T to the stream.
   Assume that T is already in encoder cache.
   This is used to stream tree bodies where we know the DFS walk arranged
   everything to cache.  Must be matched with stream_read_tree_ref.  */

void
stream_write_tree_ref (struct output_block *ob, tree t)
{
  if (!t)
    streamer_write_zero (ob);
  else
    {
      unsigned int ix;
      bool existed_p = streamer_tree_cache_lookup (ob->writer_cache, t, &ix);
      if (existed_p)
	streamer_write_hwi (ob, ix + 1);
      else
	{
	  enum LTO_tags tag;
	  unsigned ix;
	  int id = 0;

	  lto_indexable_tree_ref (ob, t, &tag, &ix);
	  if (tag == LTO_ssa_name_ref)
	    id = 1;
	  else
	    gcc_checking_assert (tag == LTO_global_stream_ref);
	  streamer_write_hwi (ob, -(int)(ix * 2 + id + 1));
	}
    }
}



/* Write a physical representation of tree node EXPR to output block
   OB.  If REF_P is true, the leaves of EXPR are emitted as references
   via lto_output_tree_ref.  IX is the index into the streamer cache
   where EXPR is stored.  */

static void
lto_write_tree_1 (struct output_block *ob, tree expr, bool ref_p)
{
  if (streamer_dump_file)
    {
      print_node_brief (streamer_dump_file, "     Streaming body of ",
			expr, 4);
      fprintf (streamer_dump_file, "  to %s\n",
	       lto_section_name[ob->section_type]);
    }

  /* Pack all the non-pointer fields in EXPR into a bitpack and write
     the resulting bitpack.  */
  streamer_write_tree_bitfields (ob, expr);

  /* Write all the pointer fields in EXPR.  */
  streamer_write_tree_body (ob, expr);

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

  /* Stream references to early generated DIEs.  Keep in sync with the
     trees handled in dwarf2out_die_ref_for_decl.  */
  if ((DECL_P (expr)
       && TREE_CODE (expr) != FIELD_DECL
       && TREE_CODE (expr) != DEBUG_EXPR_DECL
       && TREE_CODE (expr) != TYPE_DECL)
      || TREE_CODE (expr) == BLOCK)
    {
      const char *sym;
      unsigned HOST_WIDE_INT off;
      if (debug_info_level > DINFO_LEVEL_NONE
	  && debug_hooks->die_ref_for_decl (expr, &sym, &off))
	{
	  streamer_write_string (ob, ob->main_stream, sym, true);
	  streamer_write_uhwi (ob, off);
	}
      else
	streamer_write_string (ob, ob->main_stream, NULL, true);
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
      streamer_write_integer_cst (ob, expr);
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
  auto_vec<scc_entry,32> sccstack;

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
  /* Maximum index of scc stack containing a local tree.  */
  int max_local_entry;

  static int scc_entry_compare (const void *, const void *);

  void DFS_write_tree_body (struct output_block *ob,
			    tree expr, sccs *expr_state, bool ref_p);

  void DFS_write_tree (struct output_block *ob, sccs *from_state,
		       tree expr, bool ref_p, bool this_ref_p);

  hashval_t
  hash_scc (struct output_block *ob, unsigned first, unsigned size,
	    bool ref_p, bool this_ref_p);

  hash_map<tree, sccs *> sccstate;
  auto_vec<worklist, 32> worklist_vec;
  struct obstack sccstate_obstack;
};

/* Return true if type can not be merged with structurally same tree in
   other translation unit.  During stream out this information is propagated
   to all trees referring to T and they are not streamed with additional
   information needed by the tree merging in lto-common.cc (in particular,
   scc hash codes are not streamed).

   TRANSLATION_UNIT_DECL is handled specially since references to it does
   not make other trees local as well.  */

static bool
local_tree_p (tree t)
{
  switch (TREE_CODE (t))
    {
    case LABEL_DECL:
      return true;
    case NAMESPACE_DECL:
      return !DECL_NAME (t);
    case VAR_DECL:
    case FUNCTION_DECL:
      return !TREE_PUBLIC (t) && !DECL_EXTERNAL (t);
    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      /* Anonymous namespace types are local.
	 Only work hard for main variants;
	 variant types will inherit locality.  */
      return TYPE_MAIN_VARIANT (t) == t
	     && odr_type_p (t) && type_with_linkage_p (t)
	     && type_in_anonymous_namespace_p (t);
    default:
      return false;
    }
}

/* Emit the physical representation of tree node EXPR to output block OB,
   using depth-first search on the subgraph.  If THIS_REF_P is true, the
   leaves of EXPR are emitted as references via lto_output_tree_ref.
   REF_P is used for streaming siblings of EXPR.  If SINGLE_P is true,
   this is for a rewalk of a single leaf SCC.  */

DFS::DFS (struct output_block *ob, tree expr, bool ref_p, bool this_ref_p,
	  bool single_p)
{
  unsigned int next_dfs_num = 1;

  max_local_entry = -1;
  gcc_obstack_init (&sccstate_obstack);
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
	  if (ob->local_trees && local_tree_p (expr))
	    max_local_entry = sccstack.length ();
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
	  bool local_to_unit = !ob->local_trees
			       || max_local_entry >= (int)first;

	  /* Remember that trees are local so info gets propagated to other
	     SCCs.  */
	  if (local_to_unit && ob->local_trees)
	    {
	      for (unsigned i = 0; i < size; ++i)
		ob->local_trees->add (sccstack[first + i].t);
	    }

	  /* As a special case do not stream TRANSLATION_UNIT_DECL as shared
	     tree.  We can not mark it local because references to it does not
	     make other trees local (all global decls reffer to it via
	     CONTEXT).  */
	  if (size == 1
	      && TREE_CODE (sccstack[first].t) == TRANSLATION_UNIT_DECL)
	    local_to_unit = true;

	  if (!local_to_unit)
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

	  worklist_vec.pop ();

	  unsigned int prev_size = ob->main_stream->total_size;

	  /* Only global decl sections are considered by tree merging.  */
	  if (ob->section_type != LTO_section_decls)
	    {
	      /* If this is the original tree we stream and it forms SCC
		 by itself then we do not need to stream SCC at all.  */
	      if (worklist_vec.is_empty () && first == 0 && size == 1)
		 return;
	      if (streamer_dump_file)
		{
		  fprintf (streamer_dump_file,
			   "     Start of LTO_trees of size %i\n", size);
		}
	      streamer_write_record_start (ob, LTO_trees);
	      streamer_write_uhwi (ob, size);
	    }
	  /* Write LTO_tree_scc if tree merging is going to be performed.  */
	  else if (!local_to_unit
		   /* These are special since sharing is not done by tree
		      merging machinery.  We can not special case them earlier
		      because we still need to compute hash for further sharing
		      of trees referring to them.  */
		   && (size != 1
		       || (TREE_CODE (sccstack[first].t) != IDENTIFIER_NODE
			   && (TREE_CODE (sccstack[first].t) != INTEGER_CST
			       || TREE_OVERFLOW (sccstack[first].t)))))

	    {
	      gcc_checking_assert (ob->section_type == LTO_section_decls);
	      if (streamer_dump_file)
		{
		  fprintf (streamer_dump_file,
			   "     Start of LTO_tree_scc of size %i\n", size);
		}
	      streamer_write_record_start (ob, LTO_tree_scc);
	      /* In wast majority of cases scc_entry_len is 1 and size is small
		 integer.  Use extra bit of size to stream info about
		 exceptions.  */
	      streamer_write_uhwi (ob, size * 2 + (scc_entry_len != 1));
	      if (scc_entry_len != 1)
		streamer_write_uhwi (ob, scc_entry_len);
	      streamer_write_uhwi (ob, scc_hash);
	    }
	  /* Non-trivial SCCs must be packed to trees blocks so forward
	     references work correctly.  */
	  else if (size != 1)
	    {
	      if (streamer_dump_file)
		{
		  fprintf (streamer_dump_file,
			   "     Start of LTO_trees of size %i\n", size);
		}
	      streamer_write_record_start (ob, LTO_trees);
	      streamer_write_uhwi (ob, size);
	    }
	  else if (streamer_dump_file)
	    {
	      fprintf (streamer_dump_file, "     Streaming single tree\n");
	    }

	  /* Write size-1 SCCs without wrapping them inside SCC bundles.
	     All INTEGER_CSTs need to be handled this way as we need
	     their type to materialize them.  Also builtins are handled
	     this way.  */
	  if (size == 1)
	    lto_output_tree_1 (ob, expr, scc_hash, ref_p, this_ref_p);
	  else
	    {

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
		lto_write_tree_1 (ob, sccstack[first+i].t, ref_p);
	    }
	  if (streamer_dump_file)
	    fprintf (streamer_dump_file, "     %u bytes\n",
		     ob->main_stream->total_size - prev_size);

	  /* Finally truncate the vector.  */
	  sccstack.truncate (first);
	  if ((int)first <= max_local_entry)
	    max_local_entry = first - 1;

	  if (from_state)
	    from_state->low = MIN (from_state->low, cstate->low);
	  continue;
	}

      gcc_checking_assert (from_state);
      from_state->low = MIN (from_state->low, cstate->low);
      if (cstate->dfsnum < from_state->dfsnum)
	from_state->low = MIN (cstate->dfsnum, from_state->low);
      worklist_vec.pop ();
    }
}

DFS::~DFS ()
{
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
      unsigned int count = vector_cst_encoded_nelts (expr);
      for (unsigned int i = 0; i < count; ++i)
	DFS_follow_tree_edge (VECTOR_CST_ENCODED_ELT (expr, i));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_POLY_INT_CST))
    for (unsigned int i = 0; i < NUM_POLY_INT_COEFFS; ++i)
      DFS_follow_tree_edge (POLY_INT_CST_COEFF (expr, i));

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
	  && IDENTIFIER_ANON_P (DECL_NAME (expr)))
	;
      else
	DFS_follow_tree_edge (DECL_NAME (expr));
      if (TREE_CODE (expr) != TRANSLATION_UNIT_DECL
	  && ! DECL_CONTEXT (expr))
	DFS_follow_tree_edge ((*all_translation_units)[0]);
      else
	DFS_follow_tree_edge (DECL_CONTEXT (expr));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    {
      DFS_follow_tree_edge (DECL_SIZE (expr));
      DFS_follow_tree_edge (DECL_SIZE_UNIT (expr));

      /* Note, DECL_INITIAL is not handled here.  Since DECL_INITIAL needs
	 special handling in LTO, it must be handled by streamer hooks.  */

      DFS_follow_tree_edge (DECL_ATTRIBUTES (expr));

      /* We use DECL_ABSTRACT_ORIGIN == error_mark_node to mark
	 declarations which should be eliminated by decl merging. Be sure none
	 leaks to this point.  */
      gcc_assert (DECL_ABSTRACT_ORIGIN (expr) != error_mark_node);
      DFS_follow_tree_edge (DECL_ABSTRACT_ORIGIN (expr));

      if ((VAR_P (expr)
	   || TREE_CODE (expr) == PARM_DECL)
	  && DECL_HAS_VALUE_EXPR_P (expr))
	DFS_follow_tree_edge (DECL_VALUE_EXPR (expr));
      if (VAR_P (expr)
	  && DECL_HAS_DEBUG_EXPR_P (expr))
	DFS_follow_tree_edge (DECL_DEBUG_EXPR (expr));
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
      gcc_checking_assert (!DECL_FCONTEXT (expr));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    {
      gcc_checking_assert (DECL_VINDEX (expr) == NULL);
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
      /* Do not stream TYPE_STUB_DECL; it is not needed by LTO but currently
	 it cannot be freed by free_lang_data without triggering ICEs in
	 langhooks.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_NON_COMMON))
    {
      if (TREE_CODE (expr) == ARRAY_TYPE)
	DFS_follow_tree_edge (TYPE_DOMAIN (expr));
      else if (RECORD_OR_UNION_TYPE_P (expr))
	for (tree t = TYPE_FIELDS (expr); t; t = TREE_CHAIN (t))
	  DFS_follow_tree_edge (t);
      else if (FUNC_OR_METHOD_TYPE_P (expr))
	DFS_follow_tree_edge (TYPE_ARG_TYPES (expr));

      if (!POINTER_TYPE_P (expr))
	DFS_follow_tree_edge (TYPE_MIN_VALUE_RAW (expr));
      DFS_follow_tree_edge (TYPE_MAX_VALUE_RAW (expr));
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
	{
	  /* We would have to stream externals in the block chain as
	     non-references but we should have dropped them in
	     free-lang-data.  */
	  gcc_assert (!VAR_OR_FUNCTION_DECL_P (t) || !DECL_EXTERNAL (t));
	  DFS_follow_tree_edge (t);
	}

      DFS_follow_tree_edge (BLOCK_SUPERCONTEXT (expr));
      DFS_follow_tree_edge (BLOCK_ABSTRACT_ORIGIN (expr));

      /* Do not follow BLOCK_NONLOCALIZED_VARS.  We cannot handle debug
	 information for early inlined BLOCKs so drop it on the floor instead
	 of ICEing in dwarf2out.cc.  */

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

      /* Do not walk BINFO_INHERITANCE_CHAIN, BINFO_SUBVTT_INDEX,
	 BINFO_BASE_ACCESSES and BINFO_VPTR_INDEX; these are used
	 by C++ FE only.  */
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

  if (code == RAW_DATA_CST)
    DFS_follow_tree_edge (RAW_DATA_OWNER (expr));

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
    hstate.add_wide_int (wi::to_widest (t));

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
      hstate.add_hwi (DECL_MODE (t));
      hstate.add_flag (DECL_NONLOCAL (t));
      hstate.add_flag (DECL_VIRTUAL_P (t));
      hstate.add_flag (DECL_IGNORED_P (t));
      hstate.add_flag (DECL_ABSTRACT_P (t));
      hstate.add_flag (DECL_ARTIFICIAL (t));
      hstate.add_flag (DECL_USER_ALIGN (t));
      hstate.add_flag (DECL_PRESERVE_P (t));
      hstate.add_flag (DECL_EXTERNAL (t));
      hstate.add_flag (DECL_NOT_GIMPLE_REG_P (t));
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
	  hstate.add_flag (DECL_PADDING_P (t));
	  if (DECL_BIT_FIELD (t))
	    hstate.add_flag (DECL_FIELD_CXX_ZERO_WIDTH_BIT_FIELD (t));
	  else
	    hstate.add_flag (DECL_FIELD_ABI_IGNORED (t));
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
      hstate.add_flag (FUNCTION_DECL_DECL_TYPE (t));
      hstate.add_flag (DECL_UNINLINABLE (t));
      hstate.add_flag (DECL_POSSIBLY_INLINED (t));
      hstate.add_flag (DECL_IS_NOVOPS (t));
      hstate.add_flag (DECL_IS_RETURNS_TWICE (t));
      hstate.add_flag (DECL_IS_MALLOC (t));
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
	hstate.add_int (DECL_UNCHECKED_FUNCTION_CODE (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    {
      hstate.add_hwi (TYPE_MODE (t));
      /* TYPE_NO_FORCE_BLK is private to stor-layout and need
	 no streaming.  */
      hstate.add_flag (TYPE_PACKED (t));
      hstate.add_flag (TYPE_RESTRICT (t));
      hstate.add_flag (TYPE_USER_ALIGN (t));
      hstate.add_flag (TYPE_READONLY (t));
      if (RECORD_OR_UNION_TYPE_P (t))
	{
	  hstate.add_flag (TYPE_TRANSPARENT_AGGR (t));
	  hstate.add_flag (TYPE_FINAL_P (t));
          hstate.add_flag (TYPE_CXX_ODR_P (t));
	}
      else if (code == ARRAY_TYPE)
	hstate.add_flag (TYPE_NONALIASED_COMPONENT (t));
      if (code == ARRAY_TYPE || code == INTEGER_TYPE)
        hstate.add_flag (TYPE_STRING_FLAG (t));
      if (AGGREGATE_TYPE_P (t))
	hstate.add_flag (TYPE_TYPELESS_STORAGE (t));
      hstate.commit_flag ();
      hstate.add_int (TYPE_PRECISION_RAW (t));
      hstate.add_int (TYPE_ALIGN (t));
      hstate.add_int (TYPE_EMPTY_P (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TRANSLATION_UNIT_DECL))
    hstate.add (TRANSLATION_UNIT_LANGUAGE (t),
			strlen (TRANSLATION_UNIT_LANGUAGE (t)));

  if (CODE_CONTAINS_STRUCT (code, TS_TARGET_OPTION)
      /* We don't stream these when passing things to a different target.  */
      && !lto_stream_offload_p)
    hstate.add_hwi (cl_target_option_hash (TREE_TARGET_OPTION (t)));

  if (CODE_CONTAINS_STRUCT (code, TS_OPTIMIZATION))
    hstate.add_hwi (cl_optimization_hash (TREE_OPTIMIZATION (t)));

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
    {
      unsigned int count = vector_cst_encoded_nelts (t);
      for (unsigned int i = 0; i < count; ++i)
	visit (VECTOR_CST_ENCODED_ELT (t, i));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_POLY_INT_CST))
    for (unsigned int i = 0; i < NUM_POLY_INT_COEFFS; ++i)
      visit (POLY_INT_CST_COEFF (t, i));

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
	  && IDENTIFIER_ANON_P (DECL_NAME (t)))
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
    }

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    {
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
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_NON_COMMON))
    {
      if (code == ARRAY_TYPE)
	visit (TYPE_DOMAIN (t));
      else if (RECORD_OR_UNION_TYPE_P (t))
	for (tree f = TYPE_FIELDS (t); f; f = TREE_CHAIN (f))
	  visit (f);
      else if (code == FUNCTION_TYPE
	       || code == METHOD_TYPE)
	visit (TYPE_ARG_TYPES (t));
      if (!POINTER_TYPE_P (t))
	visit (TYPE_MIN_VALUE_RAW (t));
      visit (TYPE_MAX_VALUE_RAW (t));
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
      hstate.add_hwi (TREE_OPERAND_LENGTH (t));
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
      /* Do not walk BINFO_INHERITANCE_CHAIN, BINFO_SUBVTT_INDEX
	 BINFO_BASE_ACCESSES and BINFO_VPTR_INDEX; these are used
	 by C++ FE only.  */
    }

  if (CODE_CONTAINS_STRUCT (code, TS_CONSTRUCTOR))
    {
      unsigned i;
      tree index, value;
      hstate.add_hwi (CONSTRUCTOR_NELTS (t));
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

      hstate.add_hwi (OMP_CLAUSE_CODE (t));
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
	case OMP_CLAUSE_DOACROSS:
	  val = OMP_CLAUSE_DOACROSS_KIND (t);
	  break;
	case OMP_CLAUSE_MAP:
	  val = OMP_CLAUSE_MAP_KIND (t);
	  break;
	case OMP_CLAUSE_PROC_BIND:
	  val = OMP_CLAUSE_PROC_BIND_KIND (t);
	  break;
	case OMP_CLAUSE_REDUCTION:
	case OMP_CLAUSE_TASK_REDUCTION:
	case OMP_CLAUSE_IN_REDUCTION:
	  val = OMP_CLAUSE_REDUCTION_CODE (t);
	  break;
	default:
	  val = 0;
	  break;
	}
      hstate.add_hwi (val);
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
    {
      /* Reference to a local tree makes entry also local.  We always process
	 top of stack entry, so set max to number of entries in stack - 1.  */
      if (ob->local_trees
	  && ob->local_trees->contains (expr))
	max_local_entry = sccstack.length () - 1;
      return;
    }

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
  unsigned int size = ob->main_stream->total_size;
  /* This is the first time we see EXPR, write all reachable
     trees to OB.  */
  static bool in_dfs_walk;

  if (expr == NULL_TREE)
    {
      streamer_write_record_start (ob, LTO_null);
      return;
    }

  if (this_ref_p && tree_is_indexable (expr))
    {
      enum LTO_tags tag;
      unsigned ix;

      lto_indexable_tree_ref (ob, expr, &tag, &ix);
      streamer_write_record_start (ob, tag);
      streamer_write_uhwi (ob, ix);
      return;
    }

  existed_p = streamer_tree_cache_lookup (ob->writer_cache, expr, &ix);
  if (existed_p)
    {
      if (streamer_dump_file)
	{
	  if (in_dfs_walk)
	    print_node_brief (streamer_dump_file, "     Streaming ref to ",
			      expr, 4);
	  else
	    print_node_brief (streamer_dump_file, "   Streaming ref to ",
			      expr, 4);
	  fprintf (streamer_dump_file, "\n");
	}
      /* If a node has already been streamed out, make sure that
	 we don't write it more than once.  Otherwise, the reader
	 will instantiate two different nodes for the same object.  */
      streamer_write_record_start (ob, LTO_tree_pickle_reference);
      streamer_write_uhwi (ob, ix);
      lto_stats.num_pickle_refs_output++;
    }
  else
    {
      /* Protect against recursion which means disconnect between
	 what tree edges we walk in the DFS walk and what edges
	 we stream out.  */
      gcc_assert (!in_dfs_walk);

      if (streamer_dump_file)
	{
	  print_node_brief (streamer_dump_file, "   Streaming tree ",
			    expr, 4);
	  fprintf (streamer_dump_file, "\n");
	}

      /* Start the DFS walk.  */
      /* Save ob state ... */
      /* let's see ... */
      in_dfs_walk = true;
      DFS (ob, expr, ref_p, this_ref_p, false);

      /* Finally append a reference to the tree we were writing.  */
      existed_p = streamer_tree_cache_lookup (ob->writer_cache, expr, &ix);

      /* DFS walk above possibly skipped streaming EXPR itself to let us inline
	 it.  */
      if (!existed_p)
	lto_output_tree_1 (ob, expr, 0, ref_p, this_ref_p);
      else if (this_ref_p)
	{
	  if (streamer_dump_file)
	    {
	      print_node_brief (streamer_dump_file,
				"   Streaming final ref to ",
				expr, 4);
	      fprintf (streamer_dump_file, "\n");
	    }
	  streamer_write_record_start (ob, LTO_tree_pickle_reference);
	  streamer_write_uhwi (ob, ix);
	}
      in_dfs_walk = false;
      lto_stats.num_pickle_refs_output++;
    }
  if (streamer_dump_file && !in_dfs_walk)
    fprintf (streamer_dump_file, "    %u bytes\n",
	     ob->main_stream->total_size - size);
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
	  bitpack_d bp = bitpack_create (ob->main_stream);
	  bp_pack_var_len_unsigned (&bp, e->dest->index);
	  bp_pack_var_len_unsigned (&bp, e->flags);
	  stream_output_location_and_block (ob, &bp, e->goto_locus);
	  e->probability.stream_out (ob);
	}
    }

  streamer_write_hwi (ob, -1);

  bb = ENTRY_BLOCK_PTR_FOR_FN (fn);
  while (bb->next_bb)
    {
      streamer_write_hwi (ob, bb->next_bb->index);
      bb = bb->next_bb;
    }

  streamer_write_hwi (ob, -1);

  /* Output the number of loops.  */
  streamer_write_uhwi (ob, number_of_loops (fn));

  /* Output each loop, skipping the tree root which has number zero.  */
  for (unsigned i = 1; i < number_of_loops (fn); ++i)
    {
      class loop *loop = get_loop (fn, i);

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
	{
	  widest_int w = widest_int::from (loop->nb_iterations_upper_bound,
					   SIGNED);
	  streamer_write_widest_int (ob, w);
	}
      streamer_write_hwi (ob, loop->any_likely_upper_bound);
      if (loop->any_likely_upper_bound)
	{
	  widest_int w
	    = widest_int::from (loop->nb_iterations_likely_upper_bound,
				SIGNED);
	  streamer_write_widest_int (ob, w);
	}
      streamer_write_hwi (ob, loop->any_estimate);
      if (loop->any_estimate)
	{
	  widest_int w = widest_int::from (loop->nb_iterations_estimate,
					   SIGNED);
	  streamer_write_widest_int (ob, w);
	}

      /* Write OMP SIMD related info.  */
      streamer_write_hwi (ob, loop->safelen);
      streamer_write_hwi (ob, loop->unroll);
      streamer_write_hwi (ob, loop->owned_clique);
      streamer_write_hwi (ob, loop->dont_vectorize);
      streamer_write_hwi (ob, loop->force_vectorize);
      streamer_write_hwi (ob, loop->finite_p);
      stream_write_tree (ob, loop->simduid, true);
    }

  ob->main_stream = tmp_stream;
}

/* Create the header in the file using OB.  If the section type is for
   a function, set FN to the decl for that function.  */

void
produce_symbol_asm (struct output_block *ob, tree fn, int output_order)
{
  enum lto_section_type section_type = ob->section_type;
  struct lto_function_header header;
  char *section_name;

  if (section_type == LTO_section_function_body)
    {
      const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fn));
      section_name = lto_get_section_name (section_type, name,
					   output_order, NULL);
    }
  else
    section_name = lto_get_section_name (section_type, NULL, 0, NULL);

  lto_begin_section (section_name, !flag_wpa);
  free (section_name);

  /* The entire header is stream computed here.  */
  memset (&header, 0, sizeof (struct lto_function_header));

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

/* Wrapper for unused arguments.  */

void
produce_asm (struct output_block *ob)
{
  produce_symbol_asm (ob, NULL, -1);
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
  bp_pack_value (&bp, fn->calls_eh_return, 1);
  bp_pack_value (&bp, fn->has_force_vectorize_loops, 1);
  bp_pack_value (&bp, fn->has_simduid_loops, 1);
  bp_pack_value (&bp, fn->has_musttail, 1);
  bp_pack_value (&bp, fn->has_unroll, 1);
  bp_pack_value (&bp, fn->assume_function, 1);
  bp_pack_value (&bp, fn->va_list_fpr_size, 8);
  bp_pack_value (&bp, fn->va_list_gpr_size, 8);
  bp_pack_value (&bp, fn->last_clique, sizeof (short) * 8);

  /* Output the function start and end loci.  */
  stream_output_location (ob, &bp, fn->function_start_locus);
  stream_output_location (ob, &bp, fn->function_end_locus);

  /* Save the instance discriminator if present.  */
  int *instance_number_p = NULL;
  if (decl_to_instance_map)
    instance_number_p = decl_to_instance_map->get (fn->decl);
  bp_pack_value (&bp, !!instance_number_p, 1);
  if (instance_number_p)
    bp_pack_value (&bp, *instance_number_p, sizeof (int) * CHAR_BIT);

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
      collect_block_tree_leafs (root, leafs);
}

/* This performs function body modifications that are needed for streaming
   to work.  */

void
lto_prepare_function_for_streaming (struct cgraph_node *node)
{
  struct function *fn = DECL_STRUCT_FUNCTION (node->decl);
  basic_block bb;

  if (number_of_loops (fn))
    {
      push_cfun (fn);
      loop_optimizer_init (AVOID_CFG_MODIFICATIONS);
      loop_optimizer_finalize ();
      pop_cfun ();
    }
  /* We will renumber the statements.  The code that does this uses
     the same ordering that we use for serializing them so we can use
     the same code on the other end and not have to write out the
     statement numbers.  We do not assign UIDs to PHIs here because
     virtual PHIs get re-computed on-the-fly which would make numbers
     inconsistent.  */
  set_gimple_stmt_max_uid (fn, 0);
  FOR_ALL_BB_FN (bb, fn)
    {
      for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gphi *stmt = gsi.phi ();

	  /* Virtual PHIs are not going to be streamed.  */
	  if (!virtual_operand_p (gimple_phi_result (stmt)))
	    gimple_set_uid (stmt, inc_gimple_stmt_max_uid (fn));
	}
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  gimple_set_uid (stmt, inc_gimple_stmt_max_uid (fn));
	}
    }
  /* To avoid keeping duplicate gimple IDs in the statements, renumber
     virtual phis now.  */
  FOR_ALL_BB_FN (bb, fn)
    {
      for (gphi_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  gphi *stmt = gsi.phi ();
	  if (virtual_operand_p (gimple_phi_result (stmt)))
	    gimple_set_uid (stmt, inc_gimple_stmt_max_uid (fn));
	}
    }

}

/* Emit the chain of tree nodes starting at T.  OB is the output block
   to write to.  REF_P is true if chain elements should be emitted
   as references.  */

static void
streamer_write_chain (struct output_block *ob, tree t, bool ref_p)
{
  while (t)
    {
      /* We avoid outputting external vars or functions by reference
	 to the global decls section as we do not want to have them
	 enter decl merging.  We should not need to do this anymore because
	 free_lang_data removes them from block scopes.  */
      gcc_assert (!VAR_OR_FUNCTION_DECL_P (t) || !DECL_EXTERNAL (t));
      stream_write_tree (ob, t, ref_p);

      t = TREE_CHAIN (t);
    }

  /* Write a sentinel to terminate the chain.  */
  stream_write_tree (ob, NULL_TREE, ref_p);
}

/* Output the body of function NODE->DECL.  */

static void
output_function (struct cgraph_node *node, int output_order)
{
  tree function;
  struct function *fn;
  basic_block bb;
  struct output_block *ob;

  if (streamer_dump_file)
    fprintf (streamer_dump_file, "\nStreaming body of %s\n",
	     node->dump_name ());

  function = node->decl;
  fn = DECL_STRUCT_FUNCTION (function);
  ob = create_output_block (LTO_section_function_body);

  ob->symbol = node;

  gcc_assert (current_function_decl == NULL_TREE && cfun == NULL);

  /* Make string 0 be a NULL string.  */
  streamer_write_char_stream (ob->string_stream, 0);

  streamer_write_record_start (ob, LTO_function);

  /* Output decls for parameters and args.  */
  stream_write_tree (ob, DECL_RESULT (function), true);
  streamer_write_chain (ob, DECL_ARGUMENTS (function), true);

  /* Output debug args if available. */
  vec<tree, va_gc> **debugargs = decl_debug_args_lookup (function);
  if (! debugargs)
    streamer_write_uhwi (ob, 0);
  else
    {
      streamer_write_uhwi (ob, (*debugargs)->length ());
      for (unsigned i = 0; i < (*debugargs)->length (); ++i)
	stream_write_tree (ob, (**debugargs)[i], true);
    }

  /* Output DECL_INITIAL for the function, which contains the tree of
     lexical scopes.  */
  stream_write_tree (ob, DECL_INITIAL (function), true);
  /* As we do not recurse into BLOCK_SUBBLOCKS but only BLOCK_SUPERCONTEXT
     collect block tree leafs and stream those.  */
  auto_vec<tree> block_tree_leafs;
  if (DECL_INITIAL (function) && DECL_INITIAL (function) != error_mark_node)
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

      output_cfg (ob, fn);

      /* Output all the SSA names used in the function.  */
      output_ssa_names (ob, fn);

      /* Output any exception handling regions.  */
      output_eh_regions (ob, fn);

      /* Output the code for the function.  */
      FOR_ALL_BB_FN (bb, fn)
	output_bb (ob, bb, fn);

      /* The terminator for this function.  */
      streamer_write_record_start (ob, LTO_null);
   }
  else
    streamer_write_uhwi (ob, 0);

  /* Create a section to hold the pickled output of this function.   */
  produce_symbol_asm (ob, function, output_order);

  destroy_output_block (ob);
  if (streamer_dump_file)
    fprintf (streamer_dump_file, "Finished streaming %s\n",
	     node->dump_name ());
}

/* Output the body of function NODE->DECL.  */

static void
output_constructor (struct varpool_node *node, int output_order)
{
  tree var = node->decl;
  struct output_block *ob;

  if (streamer_dump_file)
    fprintf (streamer_dump_file, "\nStreaming constructor of %s\n",
	     node->dump_name ());

  timevar_push (TV_IPA_LTO_CTORS_OUT);
  ob = create_output_block (LTO_section_function_body);

  ob->symbol = node;

  /* Make string 0 be a NULL string.  */
  streamer_write_char_stream (ob->string_stream, 0);

  /* Output DECL_INITIAL for the function, which contains the tree of
     lexical scopes.  */
  stream_write_tree (ob, DECL_INITIAL (var), true);

  /* Create a section to hold the pickled output of this function.   */
  produce_symbol_asm (ob, var, output_order);

  destroy_output_block (ob);
  if (streamer_dump_file)
    fprintf (streamer_dump_file, "Finished streaming %s\n",
	     node->dump_name ());
  timevar_pop (TV_IPA_LTO_CTORS_OUT);
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
      if (TREE_CODE (can->asm_str) != STRING_CST)
	{
	  sorry_at (EXPR_LOCATION (can->asm_str),
		    "LTO streaming of toplevel extended %<asm%> "
		    "unimplemented");
	  continue;
	}
      streamer_write_string_cst (ob, ob->main_stream, can->asm_str);
      streamer_write_hwi (ob, can->order);
    }

  streamer_write_string_cst (ob, ob->main_stream, NULL_TREE);

  section_name = lto_get_section_name (LTO_section_asm, NULL, 0, NULL);
  lto_begin_section (section_name, !flag_wpa);
  free (section_name);

  /* The entire header stream is computed here.  */
  memset (&header, 0, sizeof (header));

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
copy_function_or_variable (struct symtab_node *node, int output_order)
{
  tree function = node->decl;
  struct lto_file_decl_data *file_data = node->lto_file_data;
  const char *data;
  size_t len;
  const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (function));
  char *section_name =
    lto_get_section_name (LTO_section_function_body, name, output_order, NULL);
  size_t i, j;
  struct lto_in_decl_state *in_state;
  struct lto_out_decl_state *out_state = lto_get_out_decl_state ();

  if (streamer_dump_file)
    fprintf (streamer_dump_file, "Copying section for %s\n", name);
  lto_begin_section (section_name, false);
  free (section_name);

  /* We may have renamed the declaration, e.g., a static function.  */
  name = lto_get_decl_name_mapping (file_data, name);

  data = lto_get_raw_section_data (file_data, LTO_section_function_body,
				   name, node->order - file_data->order_base,
				   &len);
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
      && VAR_P (TREE_OPERAND (t, 0))
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

/* Remove functions that are no longer used from offload_funcs, and mark the
   remaining ones with DECL_PRESERVE_P.  */

static void
prune_offload_funcs (void)
{
  if (!offload_funcs)
    return;

  unsigned ix, ix2;
  tree *elem_ptr;
  VEC_ORDERED_REMOVE_IF (*offload_funcs, ix, ix2, elem_ptr,
			 cgraph_node::get (*elem_ptr) == NULL);

  tree fn_decl;
  FOR_EACH_VEC_ELT (*offload_funcs, ix, fn_decl)
    DECL_PRESERVE_P (fn_decl) = 1;
}

/* Produce LTO section that contains global information
   about LTO bytecode.  */

static void
produce_lto_section ()
{
  /* Stream LTO meta section.  */
  output_block *ob = create_output_block (LTO_section_lto);

  char * section_name = lto_get_section_name (LTO_section_lto, NULL, 0, NULL);
  lto_begin_section (section_name, false);
  free (section_name);

#ifdef HAVE_ZSTD_H
  lto_compression compression = ZSTD;
#else
  lto_compression compression = ZLIB;
#endif

  bool slim_object = flag_generate_lto && !flag_fat_lto_objects;
  lto_section s
    = { LTO_major_version, LTO_minor_version, slim_object, 0, 0 };
  s.set_compression (compression);
  lto_write_data (&s, sizeof s);
  lto_end_section ();
  destroy_output_block (ob);
}

/* Compare symbols to get them sorted by filename (to optimize streaming)  */

static int
cmp_symbol_files (const void *pn1, const void *pn2, void *id_map_)
{
  const symtab_node *n1 = *(const symtab_node * const *)pn1;
  const symtab_node *n2 = *(const symtab_node * const *)pn2;
  hash_map<lto_file_decl_data *, int> *id_map
    = (hash_map<lto_file_decl_data *, int> *)id_map_;

  int file_order1 = n1->lto_file_data ? n1->lto_file_data->order : -1;
  int file_order2 = n2->lto_file_data ? n2->lto_file_data->order : -1;

  /* Order files same way as they appeared in the command line to reduce
     seeking while copying sections.  */
  if (file_order1 != file_order2)
    return file_order1 - file_order2;

  /* Order within static library.  */
  if (n1->lto_file_data && n1->lto_file_data->id != n2->lto_file_data->id)
    return *id_map->get (n1->lto_file_data) - *id_map->get (n2->lto_file_data);

  /* And finaly order by the definition order.  */
  return n1->order - n2->order;
}

/* Compare ints, callback for qsort.  */

static int
cmp_int (const void *a, const void *b)
{
  int ia = *(int const*) a;
  int ib = *(int const*) b;
  return ia - ib;
}

/* Create order mapping independent on symbols outside of the partition.
   Results in stable order values for incremental LTO.

   Remapping is not done in place, because symbols can be used
   by multiple partitions.  */

static void
create_order_remap (lto_symtab_encoder_t encoder)
{
  auto_vec<int> orders;
  unsigned i;
  struct asm_node* anode;
  encoder->order_remap = new hash_map<int_hash<int, -1, -2>, int>;
  unsigned n_nodes = lto_symtab_encoder_size (encoder);

  for (i = 0; i < n_nodes; i++)
    orders.safe_push (lto_symtab_encoder_deref (encoder, i)->order);

  if (!asm_nodes_output)
    {
      for (anode = symtab->first_asm_symbol (); anode; anode = anode->next)
	orders.safe_push (anode->order);
    }

  orders.qsort (cmp_int);
  int ord = 0;
  int last_order = -1;
  for (i = 0; i < orders.length (); i++)
    {
      int order = orders[i];
      if (order != last_order)
	{
	  last_order = order;
	  encoder->order_remap->put (order, ord);
	  ord++;
	}
    }

  /* Asm nodes are currently always output only into first partition.
     We can remap already here.  */
  if (!asm_nodes_output)
    {
      for (anode = symtab->first_asm_symbol (); anode; anode = anode->next)
	anode->order = *encoder->order_remap->get (anode->order);
    }
}

/* Main entry point from the pass manager.  */

void
lto_output (void)
{
  struct lto_out_decl_state *decl_state;
  bitmap output = NULL;
  bitmap_obstack output_obstack;
  unsigned int i, n_nodes;
  lto_symtab_encoder_t encoder = lto_get_out_decl_state ()->symtab_node_encoder;
  auto_vec<symtab_node *> symbols_to_copy;

  create_order_remap (encoder);

  prune_offload_funcs ();

  if (flag_checking)
    {
      bitmap_obstack_initialize (&output_obstack);
      output = BITMAP_ALLOC (&output_obstack);
    }

  /* Initialize the streamer.  */
  lto_streamer_init ();

  produce_lto_section ();

  n_nodes = lto_symtab_encoder_size (encoder);
  /* Prepare vector of functions to output and then sort it to optimize
     section copying.  */
  for (i = 0; i < n_nodes; i++)
    {
      symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
      if (snode->alias)
	continue;
      if (cgraph_node *node = dyn_cast <cgraph_node *> (snode))
	{
	  if (lto_symtab_encoder_encode_body_p (encoder, node)
	      && !node->clone_of)
	    symbols_to_copy.safe_push (node);
	}
      else if (varpool_node *node = dyn_cast <varpool_node *> (snode))
	{
	  /* Wrap symbol references inside the ctor in a type
	     preserving MEM_REF.  */
	  tree ctor = DECL_INITIAL (node->decl);
	  if (ctor && !in_lto_p)
	    walk_tree (&ctor, wrap_refs, NULL, NULL);
	  if (get_symbol_initial_value (encoder, node->decl) == error_mark_node
	      && lto_symtab_encoder_encode_initializer_p (encoder, node))
	    symbols_to_copy.safe_push (node);
	}
    }
  /* Map the section hash to an order it appears in symbols_to_copy
     since we want to sort same ID symbols next to each other but need
     to avoid making overall order depend on the actual hash value.  */
  int order = 0;
  hash_map<lto_file_decl_data *, int> id_map;
  for (i = 0; i < symbols_to_copy.length (); ++i)
    {
      symtab_node *snode = symbols_to_copy[i];
      if (snode->lto_file_data)
	{
	  bool existed_p = false;
	  int &ord = id_map.get_or_insert (snode->lto_file_data, &existed_p);
	  if (!existed_p)
	    ord = order++;
	}
    }
  symbols_to_copy.sort (cmp_symbol_files, (void *)&id_map);
  for (i = 0; i < symbols_to_copy.length (); i++)
    {
      symtab_node *snode = symbols_to_copy[i];
      cgraph_node *cnode;
      varpool_node *vnode;

      int output_order = *encoder->order_remap->get (snode->order);

      if (flag_checking)
	gcc_assert (bitmap_set_bit (output, DECL_UID (snode->decl)));

      decl_state = lto_new_out_decl_state ();
      lto_push_out_decl_state (decl_state);

      if ((cnode = dyn_cast <cgraph_node *> (snode))
	  && (gimple_has_body_p (cnode->decl)
	      || (!flag_wpa
		  && flag_incremental_link != INCREMENTAL_LINK_LTO)
	      /* Thunks have no body but they may be synthetized
		 at WPA time.  */
	      || DECL_ARGUMENTS (cnode->decl)))
	output_function (cnode, output_order);
      else if ((vnode = dyn_cast <varpool_node *> (snode))
	       && (DECL_INITIAL (vnode->decl) != error_mark_node
		   || (!flag_wpa
		       && flag_incremental_link != INCREMENTAL_LINK_LTO)))
	output_constructor (vnode, output_order);
      else
	copy_function_or_variable (snode, output_order);
      gcc_assert (lto_get_out_decl_state () == decl_state);
      lto_pop_out_decl_state ();
      lto_record_function_out_decl_state (snode->decl, decl_state);
    }

  /* Emit the callgraph after emitting function bodies.  This needs to
     be done now to make sure that all the statements in every function
     have been renumbered so that edges can be associated with call
     statements using the statement UIDs.  */
  output_symtab ();

  if (lto_get_out_decl_state ()->output_offload_tables_p)
    output_offload_tables ();

  if (flag_checking)
    {
      BITMAP_FREE (output);
      bitmap_obstack_release (&output_obstack);
    }
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
      if (streamer_dump_file)
	{
          fprintf (streamer_dump_file, " %i:", (int)index);
	  print_node_brief (streamer_dump_file, "", t, 4);
          fprintf (streamer_dump_file, "\n");
	}
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

  gcc_assert (VAR_OR_FUNCTION_DECL_P (t));

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (t));

  /* This behaves like assemble_name_raw in varasm.cc, performing the
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

/* Write extension information for symbols (symbol type, section flags).  */

static void
write_symbol_extension_info (tree t)
{
  unsigned char c;
  c = ((unsigned char) TREE_CODE (t) == VAR_DECL
       ? GCCST_VARIABLE : GCCST_FUNCTION);
  lto_write_data (&c, 1);
  unsigned char section_kind = 0;
  if (VAR_P (t))
    {
      section *s = get_variable_section (t, false);
      if (s->common.flags & SECTION_BSS)
	section_kind |= GCCSSK_BSS;
    }
  lto_write_data (&section_kind, 1);
}

/* Write an IL symbol table to OB.
   SET and VSET are cgraph/varpool node sets we are outputting.  */

static unsigned int
produce_symtab (struct output_block *ob)
{
  unsigned int streamed_symbols = 0;
  struct streamer_tree_cache_d *cache = ob->writer_cache;
  char *section_name = lto_get_section_name (LTO_section_symtab, NULL, 0, NULL);
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

      if (DECL_EXTERNAL (node->decl) || !node->output_to_lto_symbol_table_p ())
	continue;
      write_symbol (cache, node->decl, &seen, false);
      ++streamed_symbols;
    }
  for (lsei = lsei_start (encoder);
       !lsei_end_p (lsei); lsei_next (&lsei))
    {
      symtab_node *node = lsei_node (lsei);

      if (!DECL_EXTERNAL (node->decl) || !node->output_to_lto_symbol_table_p ())
	continue;
      write_symbol (cache, node->decl, &seen, false);
      ++streamed_symbols;
    }

  lto_end_section ();

  return streamed_symbols;
}

/* Symtab extension version.  */
#define LTO_SYMTAB_EXTENSION_VERSION 1

/* Write an IL symbol table extension to OB.
   SET and VSET are cgraph/varpool node sets we are outputting.  */

static void
produce_symtab_extension (struct output_block *ob,
			  unsigned int previous_streamed_symbols)
{
  unsigned int streamed_symbols = 0;
  char *section_name = lto_get_section_name (LTO_section_symtab_extension,
					     NULL, 0, NULL);
  lto_symtab_encoder_t encoder = ob->decl_state->symtab_node_encoder;
  lto_symtab_encoder_iterator lsei;

  lto_begin_section (section_name, false);
  free (section_name);

  unsigned char version = LTO_SYMTAB_EXTENSION_VERSION;
  lto_write_data (&version, 1);

  /* Write the symbol table.
     First write everything defined and then all declarations.
     This is necessary to handle cases where we have duplicated symbols.  */
  for (lsei = lsei_start (encoder);
       !lsei_end_p (lsei); lsei_next (&lsei))
    {
      symtab_node *node = lsei_node (lsei);

      if (DECL_EXTERNAL (node->decl) || !node->output_to_lto_symbol_table_p ())
	continue;
      write_symbol_extension_info (node->decl);
      ++streamed_symbols;
    }
  for (lsei = lsei_start (encoder);
       !lsei_end_p (lsei); lsei_next (&lsei))
    {
      symtab_node *node = lsei_node (lsei);

      if (!DECL_EXTERNAL (node->decl) || !node->output_to_lto_symbol_table_p ())
	continue;
      write_symbol_extension_info (node->decl);
      ++streamed_symbols;
    }

  gcc_assert (previous_streamed_symbols == streamed_symbols);
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

  if (lto_stream_offload_p)
    bp_pack_value (&bp, NUM_POLY_INT_COEFFS, MAX_NUM_POLY_INT_COEFFS_BITS);

  /* Ensure that for GET_MODE_INNER (m) != m we have
     also the inner mode marked.  */
  for (int i = 0; i < (int) MAX_MACHINE_MODE; i++)
    if (streamer_mode_table[i])
      {
	machine_mode m = (machine_mode) i;
	machine_mode inner_m = GET_MODE_INNER (m);
	if (inner_m != m)
	  streamer_mode_table[(int) inner_m] = 1;
      }

  /* Pack the mode_bits value within 5 bits (up to 31) in the beginning.  */
  unsigned mode_bits = ceil_log2 (MAX_MACHINE_MODE);
  bp_pack_value (&bp, mode_bits, 5);

  /* First stream modes that have GET_MODE_INNER (m) == m,
     so that we can refer to them afterwards.  */
  for (int pass = 0; pass < 2; pass++)
    for (int i = 0; i < (int) MAX_MACHINE_MODE; i++)
      if (streamer_mode_table[i] && i != (int) VOIDmode && i != (int) BLKmode)
	{
	  machine_mode m = (machine_mode) i;
	  if ((GET_MODE_INNER (m) == m) ^ (pass == 0))
	    continue;
	  bp_pack_value (&bp, m, mode_bits);
	  bp_pack_enum (&bp, mode_class, MAX_MODE_CLASS, GET_MODE_CLASS (m));
	  bp_pack_poly_value (&bp, GET_MODE_SIZE (m), 16);
	  bp_pack_poly_value (&bp, GET_MODE_PRECISION (m), 16);
	  bp_pack_value (&bp, GET_MODE_INNER (m), mode_bits);
	  bp_pack_poly_value (&bp, GET_MODE_NUNITS (m), 16);
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
  bp_pack_value (&bp, VOIDmode, mode_bits);

  streamer_write_bitpack (&bp);

  char *section_name
    = lto_get_section_name (LTO_section_mode_table, NULL, 0, NULL);
  lto_begin_section (section_name, !flag_wpa);
  free (section_name);

  /* The entire header stream is computed here.  */
  struct lto_simple_header_with_strings header;
  memset (&header, 0, sizeof (header));

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

  section_name = lto_get_section_name (LTO_section_decls, NULL, 0, NULL);
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
  if (streamer_dump_file)
    fprintf (streamer_dump_file, "Outputting global stream\n");
  lto_output_decl_state_streams (ob, out_state);
  num_fns = lto_function_decl_states.length ();
  for (idx = 0; idx < num_fns; idx++)
    {
      fn_out_state =
	lto_function_decl_states[idx];
      if (streamer_dump_file)
	fprintf (streamer_dump_file, "Outputting stream for %s\n",
		 IDENTIFIER_POINTER
		    (DECL_ASSEMBLER_NAME (fn_out_state->fn_decl)));
      lto_output_decl_state_streams (ob, fn_out_state);
    }

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
    {
      unsigned int streamed_symbols = produce_symtab (ob);
      produce_symtab_extension (ob, streamed_symbols);
    }

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
