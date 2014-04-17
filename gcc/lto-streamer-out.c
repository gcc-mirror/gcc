/* Write the GIMPLE representation to a file stream.

   Copyright (C) 2009-2014 Free Software Foundation, Inc.
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
#include "tm.h"
#include "tree.h"
#include "stor-layout.h"
#include "stringpool.h"
#include "expr.h"
#include "flags.h"
#include "params.h"
#include "input.h"
#include "hashtab.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "tree-ssanames.h"
#include "tree-pass.h"
#include "function.h"
#include "diagnostic-core.h"
#include "except.h"
#include "lto-symtab.h"
#include "lto-streamer.h"
#include "data-streamer.h"
#include "gimple-streamer.h"
#include "tree-streamer.h"
#include "streamer-hooks.h"
#include "cfgloop.h"


static void lto_write_tree (struct output_block*, tree, bool);

/* Clear the line info stored in DATA_IN.  */

static void
clear_line_info (struct output_block *ob)
{
  ob->current_file = NULL;
  ob->current_line = 0;
  ob->current_col = 0;
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

  ob->string_hash_table.create (37);
  gcc_obstack_init (&ob->obstack);

  return ob;
}


/* Destroy the output block OB.  */

void
destroy_output_block (struct output_block *ob)
{
  enum lto_section_type section_type = ob->section_type;

  ob->string_hash_table.dispose ();

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
  if (TREE_CODE (t) == PARM_DECL || TREE_CODE (t) == RESULT_DECL)
    return variably_modified_type_p (TREE_TYPE (DECL_CONTEXT (t)), NULL_TREE);
  else if (((TREE_CODE (t) == VAR_DECL && !TREE_STATIC (t))
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
  bp_pack_value (bp, loc == UNKNOWN_LOCATION, 1);
  if (loc == UNKNOWN_LOCATION)
    return;

  xloc = expand_location (loc);

  bp_pack_value (bp, ob->current_file != xloc.file, 1);
  bp_pack_value (bp, ob->current_line != xloc.line, 1);
  bp_pack_value (bp, ob->current_col != xloc.column, 1);

  if (ob->current_file != xloc.file)
    bp_pack_var_len_unsigned (bp,
	                      streamer_string_index (ob, xloc.file,
						     strlen (xloc.file) + 1,
						     true));
  ob->current_file = xloc.file;

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


/* For EXPR lookup and return what we want to stream to OB as DECL_INITIAL.  */

static tree
get_symbol_initial_value (struct output_block *ob, tree expr)
{
  gcc_checking_assert (DECL_P (expr)
		       && TREE_CODE (expr) != FUNCTION_DECL
		       && TREE_CODE (expr) != TRANSLATION_UNIT_DECL);

  /* Handle DECL_INITIAL for symbols.  */
  tree initial = DECL_INITIAL (expr);
  if (TREE_CODE (expr) == VAR_DECL
      && (TREE_STATIC (expr) || DECL_EXTERNAL (expr))
      && !DECL_IN_CONSTANT_POOL (expr)
      && initial)
    {
      lto_symtab_encoder_t encoder;
      varpool_node *vnode;

      encoder = ob->decl_state->symtab_node_encoder;
      vnode = varpool_get_node (expr);
      if (!vnode
	  || !lto_symtab_encoder_encode_initializer_p (encoder,
						       vnode))
	initial = error_mark_node;
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
  bitpack_d bp = bitpack_create (ob->main_stream);
  streamer_pack_tree_bitfields (ob, &bp, expr);
  streamer_write_bitpack (&bp);

  /* Write all the pointer fields in EXPR.  */
  streamer_write_tree_body (ob, expr, ref_p);

  /* Write any LTO-specific data to OB.  */
  if (DECL_P (expr)
      && TREE_CODE (expr) != FUNCTION_DECL
      && TREE_CODE (expr) != TRANSLATION_UNIT_DECL)
    {
      /* Handle DECL_INITIAL for symbols.  */
      tree initial = get_symbol_initial_value (ob, expr);
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

/* Emit the physical representation of tree node EXPR to output block
   OB.  If THIS_REF_P is true, the leaves of EXPR are emitted as references
   via lto_output_tree_ref.  REF_P is used for streaming siblings of EXPR.  */

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
  if (streamer_handle_as_builtin_p (expr))
    {
      /* MD and NORMAL builtins do not need to be written out
	 completely as they are always instantiated by the
	 compiler on startup.  The only builtins that need to
	 be written out are BUILT_IN_FRONTEND.  For all other
	 builtins, we simply write the class and code.  */
      streamer_write_builtin (ob, expr);
    }
  else if (TREE_CODE (expr) == INTEGER_CST
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

struct sccs
{
  unsigned int dfsnum;
  unsigned int low;
};

struct scc_entry
{
  tree t;
  hashval_t hash;
};

static unsigned int next_dfs_num;
static vec<scc_entry> sccstack;
static struct pointer_map_t *sccstate;
static struct obstack sccstate_obstack;

static void
DFS_write_tree (struct output_block *ob, sccs *from_state,
		tree expr, bool ref_p, bool this_ref_p);

/* Handle the tree EXPR in the DFS walk with SCC state EXPR_STATE and
   DFS recurse for all tree edges originating from it.  */

static void
DFS_write_tree_body (struct output_block *ob,
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
	  && ANON_AGGRNAME_P (DECL_NAME (expr)))
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
	 dwarf2out.c.  */

      if ((TREE_CODE (expr) == VAR_DECL
	   || TREE_CODE (expr) == PARM_DECL)
	  && DECL_HAS_VALUE_EXPR_P (expr))
	DFS_follow_tree_edge (DECL_VALUE_EXPR (expr));
      if (TREE_CODE (expr) == VAR_DECL)
	DFS_follow_tree_edge (DECL_DEBUG_EXPR (expr));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_NON_COMMON))
    {
      if (TREE_CODE (expr) == TYPE_DECL)
	DFS_follow_tree_edge (DECL_ORIGINAL_TYPE (expr));
      DFS_follow_tree_edge (DECL_VINDEX (expr));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    {
      /* Make sure we don't inadvertently set the assembler name.  */
      if (DECL_ASSEMBLER_NAME_SET_P (expr))
	DFS_follow_tree_edge (DECL_ASSEMBLER_NAME (expr));
      DFS_follow_tree_edge (DECL_SECTION_NAME (expr));
      DFS_follow_tree_edge (DECL_COMDAT_GROUP (expr));
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
      DFS_follow_tree_edge (DECL_FUNCTION_PERSONALITY (expr));
      /* Do not DECL_FUNCTION_SPECIFIC_TARGET.  They will be regenerated.  */
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
	/* ???  FIXME.  See also streamer_write_chain.  */
	if (!(VAR_OR_FUNCTION_DECL_P (t)
	      && DECL_EXTERNAL (t)))
	  DFS_follow_tree_edge (t);

      DFS_follow_tree_edge (BLOCK_SUPERCONTEXT (expr));

      /* Follow BLOCK_ABSTRACT_ORIGIN for the limited cases we can
	 handle - those that represent inlined function scopes.
	 For the drop rest them on the floor instead of ICEing
	 in dwarf2out.c.  */
      if (inlined_function_outer_scope_p (expr))
	{
	  tree ultimate_origin = block_ultimate_origin (expr);
	  DFS_follow_tree_edge (ultimate_origin);
	}
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

/* Return a hash value for the tree T.  */

static hashval_t
hash_tree (struct streamer_tree_cache_d *cache, tree t)
{
#define visit(SIBLING) \
  do { \
    unsigned ix; \
    if (SIBLING && streamer_tree_cache_lookup (cache, SIBLING, &ix)) \
      v = iterative_hash_hashval_t (streamer_tree_cache_get_hash (cache, ix), v); \
  } while (0)

  /* Hash TS_BASE.  */
  enum tree_code code = TREE_CODE (t);
  hashval_t v = iterative_hash_host_wide_int (code, 0);
  if (!TYPE_P (t))
    {
      v = iterative_hash_host_wide_int (TREE_SIDE_EFFECTS (t)
					| (TREE_CONSTANT (t) << 1)
					| (TREE_READONLY (t) << 2)
					| (TREE_PUBLIC (t) << 3), v);
    }
  v = iterative_hash_host_wide_int (TREE_ADDRESSABLE (t)
				    | (TREE_THIS_VOLATILE (t) << 1), v);
  if (DECL_P (t))
    v = iterative_hash_host_wide_int (DECL_UNSIGNED (t), v);
  else if (TYPE_P (t))
    v = iterative_hash_host_wide_int (TYPE_UNSIGNED (t), v);
  if (TYPE_P (t))
    v = iterative_hash_host_wide_int (TYPE_ARTIFICIAL (t), v);
  else
    v = iterative_hash_host_wide_int (TREE_NO_WARNING (t), v);
  v = iterative_hash_host_wide_int (TREE_NOTHROW (t)
				    | (TREE_STATIC (t) << 1)
				    | (TREE_PROTECTED (t) << 2)
				    | (TREE_DEPRECATED (t) << 3), v);
  if (code != TREE_BINFO)
    v = iterative_hash_host_wide_int (TREE_PRIVATE (t), v);
  if (TYPE_P (t))
    v = iterative_hash_host_wide_int (TYPE_SATURATING (t)
				      | (TYPE_ADDR_SPACE (t) << 1), v);
  else if (code == SSA_NAME)
    v = iterative_hash_host_wide_int (SSA_NAME_IS_DEFAULT_DEF (t), v);

  if (CODE_CONTAINS_STRUCT (code, TS_INT_CST))
    {
      int i;
      v = iterative_hash_host_wide_int (TREE_INT_CST_NUNITS (t), v);
      v = iterative_hash_host_wide_int (TREE_INT_CST_EXT_NUNITS (t), v);
      for (i = 0; i < TREE_INT_CST_NUNITS (t); i++)
	v = iterative_hash_host_wide_int (TREE_INT_CST_ELT (t, i), v);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_REAL_CST))
    {
      REAL_VALUE_TYPE r = TREE_REAL_CST (t);
      v = iterative_hash_host_wide_int (r.cl, v);
      v = iterative_hash_host_wide_int (r.decimal
					| (r.sign << 1)
					| (r.signalling << 2)
					| (r.canonical << 3), v);
      v = iterative_hash_host_wide_int (r.uexp, v);
      for (unsigned i = 0; i < SIGSZ; ++i)
	v = iterative_hash_host_wide_int (r.sig[i], v);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_FIXED_CST))
    {
      FIXED_VALUE_TYPE f = TREE_FIXED_CST (t);
      v = iterative_hash_host_wide_int (f.mode, v);
      v = iterative_hash_host_wide_int (f.data.low, v);
      v = iterative_hash_host_wide_int (f.data.high, v);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    {
      v = iterative_hash_host_wide_int (DECL_MODE (t), v);
      v = iterative_hash_host_wide_int (DECL_NONLOCAL (t)
					| (DECL_VIRTUAL_P (t) << 1)
					| (DECL_IGNORED_P (t) << 2)
					| (DECL_ABSTRACT (t) << 3)
					| (DECL_ARTIFICIAL (t) << 4)
					| (DECL_USER_ALIGN (t) << 5)
					| (DECL_PRESERVE_P (t) << 6)
					| (DECL_EXTERNAL (t) << 7)
					| (DECL_GIMPLE_REG_P (t) << 8), v);
      v = iterative_hash_host_wide_int (DECL_ALIGN (t), v);
      if (code == LABEL_DECL)
	{
	  v = iterative_hash_host_wide_int (EH_LANDING_PAD_NR (t), v);
	  v = iterative_hash_host_wide_int (LABEL_DECL_UID (t), v);
	}
      else if (code == FIELD_DECL)
	{
	  v = iterative_hash_host_wide_int (DECL_PACKED (t)
					    | (DECL_NONADDRESSABLE_P (t) << 1),
					    v);
	  v = iterative_hash_host_wide_int (DECL_OFFSET_ALIGN (t), v);
	}
      else if (code == VAR_DECL)
	{
	  v = iterative_hash_host_wide_int (DECL_HAS_DEBUG_EXPR_P (t)
					    | (DECL_NONLOCAL_FRAME (t) << 1),
					    v);
	}
      if (code == RESULT_DECL
	  || code == PARM_DECL
	  || code == VAR_DECL)
	{
	  v = iterative_hash_host_wide_int (DECL_BY_REFERENCE (t), v);
	  if (code == VAR_DECL
	      || code == PARM_DECL)
	    v = iterative_hash_host_wide_int (DECL_HAS_VALUE_EXPR_P (t), v);
	}
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WRTL))
    v = iterative_hash_host_wide_int (DECL_REGISTER (t), v);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    {
      v = iterative_hash_host_wide_int ((DECL_COMMON (t))
					| (DECL_DLLIMPORT_P (t) << 1)
					| (DECL_WEAK (t) << 2)
					| (DECL_SEEN_IN_BIND_EXPR_P (t) << 3)
					| (DECL_COMDAT (t) << 4)
					| (DECL_VISIBILITY_SPECIFIED (t) << 6),
					v);
      v = iterative_hash_host_wide_int (DECL_VISIBILITY (t), v);
      if (code == VAR_DECL)
	{
	  /* DECL_IN_TEXT_SECTION is set during final asm output only.  */
	  v = iterative_hash_host_wide_int (DECL_HARD_REGISTER (t)
					    | (DECL_IN_CONSTANT_POOL (t) << 1),
					    v);
	  v = iterative_hash_host_wide_int (DECL_TLS_MODEL (t), v);
	}
      if (TREE_CODE (t) == FUNCTION_DECL)
	v = iterative_hash_host_wide_int (DECL_FINAL_P (t)
					  | (DECL_CXX_CONSTRUCTOR_P (t) << 1)
					  | (DECL_CXX_DESTRUCTOR_P (t) << 2),
					  v);
      if (VAR_OR_FUNCTION_DECL_P (t))
	v = iterative_hash_host_wide_int (DECL_INIT_PRIORITY (t), v);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    {
      v = iterative_hash_host_wide_int (DECL_BUILT_IN_CLASS (t), v);
      v = iterative_hash_host_wide_int (DECL_STATIC_CONSTRUCTOR (t)
					| (DECL_STATIC_DESTRUCTOR (t) << 1)
					| (DECL_UNINLINABLE (t) << 2)
					| (DECL_POSSIBLY_INLINED (t) << 3)
					| (DECL_IS_NOVOPS (t) << 4)
					| (DECL_IS_RETURNS_TWICE (t) << 5)
					| (DECL_IS_MALLOC (t) << 6)
					| (DECL_IS_OPERATOR_NEW (t) << 7)
					| (DECL_DECLARED_INLINE_P (t) << 8)
					| (DECL_STATIC_CHAIN (t) << 9)
					| (DECL_NO_INLINE_WARNING_P (t) << 10)
					| (DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (t) << 11)
					| (DECL_NO_LIMIT_STACK (t) << 12)
					| (DECL_DISREGARD_INLINE_LIMITS (t) << 13)
					| (DECL_PURE_P (t) << 14)
					| (DECL_LOOPING_CONST_OR_PURE_P (t) << 15), v);
      if (DECL_BUILT_IN_CLASS (t) != NOT_BUILT_IN)
	v = iterative_hash_host_wide_int (DECL_FUNCTION_CODE (t), v);
      if (DECL_STATIC_DESTRUCTOR (t))
	v = iterative_hash_host_wide_int (DECL_FINI_PRIORITY (t), v);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE_COMMON))
    {
      v = iterative_hash_host_wide_int (TYPE_MODE (t), v);
      v = iterative_hash_host_wide_int (TYPE_STRING_FLAG (t)
					| (TYPE_NO_FORCE_BLK (t) << 1)
					| (TYPE_NEEDS_CONSTRUCTING (t) << 2)
					| (TYPE_PACKED (t) << 3)
					| (TYPE_RESTRICT (t) << 4)
					| (TYPE_USER_ALIGN (t) << 5)
					| (TYPE_READONLY (t) << 6), v);
      if (RECORD_OR_UNION_TYPE_P (t))
	{
	  v = iterative_hash_host_wide_int (TYPE_TRANSPARENT_AGGR (t)
					    | (TYPE_FINAL_P (t) << 1), v);
	}
      else if (code == ARRAY_TYPE)
	v = iterative_hash_host_wide_int (TYPE_NONALIASED_COMPONENT (t), v);
      v = iterative_hash_host_wide_int (TYPE_PRECISION (t), v);
      v = iterative_hash_host_wide_int (TYPE_ALIGN (t), v);
      v = iterative_hash_host_wide_int ((TYPE_ALIAS_SET (t) == 0
					 || (!in_lto_p
					     && get_alias_set (t) == 0))
					? 0 : -1, v);
    }

  if (CODE_CONTAINS_STRUCT (code, TS_TRANSLATION_UNIT_DECL))
    v = iterative_hash (TRANSLATION_UNIT_LANGUAGE (t),
			strlen (TRANSLATION_UNIT_LANGUAGE (t)), v);

  if (CODE_CONTAINS_STRUCT (code, TS_TARGET_OPTION))
    gcc_unreachable ();

  if (CODE_CONTAINS_STRUCT (code, TS_OPTIMIZATION))
    v = iterative_hash (t, sizeof (struct cl_optimization), v);

  if (CODE_CONTAINS_STRUCT (code, TS_IDENTIFIER))
    v = iterative_hash_host_wide_int (IDENTIFIER_HASH_VALUE (t), v);

  if (CODE_CONTAINS_STRUCT (code, TS_STRING))
    v = iterative_hash (TREE_STRING_POINTER (t), TREE_STRING_LENGTH (t), v);

  if (CODE_CONTAINS_STRUCT (code, TS_TYPED))
    {
      if (POINTER_TYPE_P (t))
	{
	  /* For pointers factor in the pointed-to type recursively as
	     we cannot recurse through only pointers.
	     ???  We can generalize this by keeping track of the
	     in-SCC edges for each tree (or arbitrarily the first
	     such edge) and hashing that in in a second stage
	     (instead of the quadratic mixing of the SCC we do now).  */
	  hashval_t x;
	  unsigned ix;
	  if (streamer_tree_cache_lookup (cache, TREE_TYPE (t), &ix))
	    x = streamer_tree_cache_get_hash (cache, ix);
	  else
	    x = hash_tree (cache, TREE_TYPE (t));
	  v = iterative_hash_hashval_t (x, v);
	}
      else if (code != IDENTIFIER_NODE)
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
	  && ANON_AGGRNAME_P (DECL_NAME (t)))
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
      visit (DECL_VINDEX (t));
    }

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    {
      if (DECL_ASSEMBLER_NAME_SET_P (t))
	visit (DECL_ASSEMBLER_NAME (t));
      visit (DECL_SECTION_NAME (t));
      visit (DECL_COMDAT_GROUP (t));
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
      visit (DECL_FUNCTION_PERSONALITY (t));
      /* Do not follow DECL_FUNCTION_SPECIFIC_TARGET.  */
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
      v = iterative_hash_host_wide_int (TREE_OPERAND_LENGTH (t), v);
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
      v = iterative_hash_host_wide_int (CONSTRUCTOR_NELTS (t), v);
      FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (t), i, index, value)
	{
	  visit (index);
	  visit (value);
	}
    }

  if (code == OMP_CLAUSE)
    {
      int i;

      v = iterative_hash_host_wide_int (OMP_CLAUSE_CODE (t), v);
      switch (OMP_CLAUSE_CODE (t))
	{
	case OMP_CLAUSE_DEFAULT:
	  v = iterative_hash_host_wide_int (OMP_CLAUSE_DEFAULT_KIND (t), v);
	  break;
	case OMP_CLAUSE_SCHEDULE:
	  v = iterative_hash_host_wide_int (OMP_CLAUSE_SCHEDULE_KIND (t), v);
	  break;
	case OMP_CLAUSE_DEPEND:
	  v = iterative_hash_host_wide_int (OMP_CLAUSE_DEPEND_KIND (t), v);
	  break;
	case OMP_CLAUSE_MAP:
	  v = iterative_hash_host_wide_int (OMP_CLAUSE_MAP_KIND (t), v);
	  break;
	case OMP_CLAUSE_PROC_BIND:
	  v = iterative_hash_host_wide_int (OMP_CLAUSE_PROC_BIND_KIND (t), v);
	  break;
	case OMP_CLAUSE_REDUCTION:
	  v = iterative_hash_host_wide_int (OMP_CLAUSE_REDUCTION_CODE (t), v);
	  break;
	default:
	  break;
	}
      for (i = 0; i < omp_clause_num_ops[OMP_CLAUSE_CODE (t)]; i++)
	visit (OMP_CLAUSE_OPERAND (t, i));
      visit (OMP_CLAUSE_CHAIN (t));
    }

  return v;

#undef visit
}

/* Compare two SCC entries by their hash value for qsorting them.  */

static int
scc_entry_compare (const void *p1_, const void *p2_)
{
  const scc_entry *p1 = (const scc_entry *) p1_;
  const scc_entry *p2 = (const scc_entry *) p2_;
  if (p1->hash < p2->hash)
    return -1;
  else if (p1->hash > p2->hash)
    return 1;
  return 0;
}

/* Return a hash value for the SCC on the SCC stack from FIRST with
   size SIZE.  */

static hashval_t
hash_scc (struct streamer_tree_cache_d *cache, unsigned first, unsigned size)
{
  /* Compute hash values for the SCC members.  */
  for (unsigned i = 0; i < size; ++i)
    sccstack[first+i].hash = hash_tree (cache, sccstack[first+i].t);

  if (size == 1)
    return sccstack[first].hash;

  /* Sort the SCC of type, hash pairs so that when we mix in
     all members of the SCC the hash value becomes independent on
     the order we visited the SCC.  Disregard hashes equal to
     the hash of the tree we mix into because we cannot guarantee
     a stable sort for those across different TUs.  */
  qsort (&sccstack[first], size, sizeof (scc_entry), scc_entry_compare);
  hashval_t *tem = XALLOCAVEC (hashval_t, size);
  for (unsigned i = 0; i < size; ++i)
    {
      hashval_t hash = sccstack[first+i].hash;
      hashval_t orig_hash = hash;
      unsigned j;
      /* Skip same hashes.  */
      for (j = i + 1;
	   j < size && sccstack[first+j].hash == orig_hash; ++j)
	;
      for (; j < size; ++j)
	hash = iterative_hash_hashval_t (sccstack[first+j].hash, hash);
      for (j = 0; sccstack[first+j].hash != orig_hash; ++j)
	hash = iterative_hash_hashval_t (sccstack[first+j].hash, hash);
      tem[i] = hash;
    }
  hashval_t scc_hash = 0;
  for (unsigned i = 0; i < size; ++i)
    {
      sccstack[first+i].hash = tem[i];
      scc_hash = iterative_hash_hashval_t (tem[i], scc_hash);
    }
  return scc_hash;
}

/* DFS walk EXPR and stream SCCs of tree bodies if they are not
   already in the streamer cache.  Main routine called for
   each visit of EXPR.  */

static void
DFS_write_tree (struct output_block *ob, sccs *from_state,
		tree expr, bool ref_p, bool this_ref_p)
{
  unsigned ix;
  sccs **slot;

  /* Handle special cases.  */
  if (expr == NULL_TREE)
    return;

  /* Do not DFS walk into indexable trees.  */
  if (this_ref_p && tree_is_indexable (expr))
    return;

  /* Check if we already streamed EXPR.  */
  if (streamer_tree_cache_lookup (ob->writer_cache, expr, &ix))
    return;

  slot = (sccs **)pointer_map_insert (sccstate, expr);
  sccs *cstate = *slot;
  if (!cstate)
    {
      scc_entry e = { expr, 0 };
      /* Not yet visited.  DFS recurse and push it onto the stack.  */
      *slot = cstate = XOBNEW (&sccstate_obstack, struct sccs);
      sccstack.safe_push (e);
      cstate->dfsnum = next_dfs_num++;
      cstate->low = cstate->dfsnum;

      if (streamer_handle_as_builtin_p (expr))
	;
      else if (TREE_CODE (expr) == INTEGER_CST
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
	      tree initial = get_symbol_initial_value (ob, expr);
	      DFS_write_tree (ob, cstate, initial, ref_p, ref_p);
	    }
	}

      /* See if we found an SCC.  */
      if (cstate->low == cstate->dfsnum)
	{
	  unsigned first, size;
	  tree x;

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
	      scc_hash = hash_scc (ob->writer_cache, first, size);

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
		{
		  scc_entry tem = sccstack[first + i];
		  sccstack[first + i] = sccstack[first + entry_start + i];
		  sccstack[first + entry_start + i] = tem;
		}
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
							      t, hash, &ix);
		  gcc_assert (!exists_p);

		  if (!lto_is_streamable (t))
		    internal_error ("tree code %qs is not supported "
				    "in LTO streams",
				    get_tree_code_name (TREE_CODE (t)));

		  gcc_checking_assert (!streamer_handle_as_builtin_p (t));

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
	  return;
	}

      if (from_state)
	from_state->low = MIN (from_state->low, cstate->low);
    }
  gcc_checking_assert (from_state);
  if (cstate->dfsnum < from_state->dfsnum)
    from_state->low = MIN (cstate->dfsnum, from_state->low);
}


/* Emit the physical representation of tree node EXPR to output block
   OB.  If THIS_REF_P is true, the leaves of EXPR are emitted as references
   via lto_output_tree_ref.  REF_P is used for streaming siblings of EXPR.  */

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
      sccstate = pointer_map_create ();
      gcc_obstack_init (&sccstate_obstack);
      next_dfs_num = 1;
      DFS_write_tree (ob, NULL, expr, ref_p, this_ref_p);
      sccstack.release ();
      pointer_map_destroy (sccstate);
      obstack_free (&sccstate_obstack, NULL);
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
	  || virtual_operand_p (ptr))
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


/* Output a wide-int.  */

static void
streamer_write_wi (struct output_block *ob,
		   const widest_int &w)
{
  int len = w.get_len ();

  streamer_write_uhwi (ob, w.get_precision ());
  streamer_write_uhwi (ob, len);
  for (int i = 0; i < len; i++)
    streamer_write_hwi (ob, w.elt (i));
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
	streamer_write_wi (ob, loop->nb_iterations_upper_bound);
      streamer_write_hwi (ob, loop->any_estimate);
      if (loop->any_estimate)
	streamer_write_wi (ob, loop->nb_iterations_estimate);

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
  struct lto_output_stream *header_stream;

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
  header.lto_header.major_version = LTO_major_version;
  header.lto_header.minor_version = LTO_minor_version;

  header.compressed_size = 0;

  if (section_type == LTO_section_function_body)
    header.cfg_size = ob->cfg_stream->total_size;
  header.main_size = ob->main_stream->total_size;
  header.string_size = ob->string_stream->total_size;

  header_stream = XCNEW (struct lto_output_stream);
  lto_output_data_stream (header_stream, &header, sizeof header);
  lto_write_stream (header_stream);
  free (header_stream);

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
  bp_pack_value (&bp, fn->calls_alloca, 1);
  bp_pack_value (&bp, fn->calls_setjmp, 1);
  bp_pack_value (&bp, fn->has_force_vectorize_loops, 1);
  bp_pack_value (&bp, fn->has_simduid_loops, 1);
  bp_pack_value (&bp, fn->va_list_fpr_size, 8);
  bp_pack_value (&bp, fn->va_list_gpr_size, 8);

  /* Output the function start and end loci.  */
  stream_output_location (ob, &bp, fn->function_start_locus);
  stream_output_location (ob, &bp, fn->function_end_locus);

  streamer_write_bitpack (&bp);
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
  ob->cgraph_node = node;

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
	  gimple_stmt_iterator gsi;
	  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple stmt = gsi_stmt (gsi);

	      /* Virtual PHIs are not going to be streamed.  */
	      if (!virtual_operand_p (gimple_phi_result (stmt)))
	        gimple_set_uid (stmt, inc_gimple_stmt_max_uid (cfun));
	    }
	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple stmt = gsi_stmt (gsi);
	      gimple_set_uid (stmt, inc_gimple_stmt_max_uid (cfun));
	    }
	}
      /* To avoid keeping duplicate gimple IDs in the statements, renumber
	 virtual phis now.  */
      FOR_ALL_BB_FN (bb, cfun)
	{
	  gimple_stmt_iterator gsi;
	  for (gsi = gsi_start_phis (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple stmt = gsi_stmt (gsi);
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


/* Emit toplevel asms.  */

void
lto_output_toplevel_asms (void)
{
  struct output_block *ob;
  struct asm_node *can;
  char *section_name;
  struct lto_output_stream *header_stream;
  struct lto_asm_header header;

  if (! asm_nodes)
    return;

  ob = create_output_block (LTO_section_asm);

  /* Make string 0 be a NULL string.  */
  streamer_write_char_stream (ob->string_stream, 0);

  for (can = asm_nodes; can; can = can->next)
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
  header.lto_header.major_version = LTO_major_version;
  header.lto_header.minor_version = LTO_minor_version;

  header.main_size = ob->main_stream->total_size;
  header.string_size = ob->string_stream->total_size;

  header_stream = XCNEW (struct lto_output_stream);
  lto_output_data_stream (header_stream, &header, sizeof (header));
  lto_write_stream (header_stream);
  free (header_stream);

  /* Put all of the gimple and the string table out the asm file as a
     block of text.  */
  lto_write_stream (ob->main_stream);
  lto_write_stream (ob->string_stream);

  lto_end_section ();

  destroy_output_block (ob);
}


/* Copy the function body of NODE without deserializing. */

static void
copy_function (struct cgraph_node *node)
{
  tree function = node->decl;
  struct lto_file_decl_data *file_data = node->lto_file_data;
  struct lto_output_stream *output_stream = XCNEW (struct lto_output_stream);
  const char *data;
  size_t len;
  const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (function));
  char *section_name =
    lto_get_section_name (LTO_section_function_body, name, NULL);
  size_t i, j;
  struct lto_in_decl_state *in_state;
  struct lto_out_decl_state *out_state = lto_get_out_decl_state ();

  lto_begin_section (section_name, !flag_wpa);
  free (section_name);

  /* We may have renamed the declaration, e.g., a static function.  */
  name = lto_get_decl_name_mapping (file_data, name);

  data = lto_get_section_data (file_data, LTO_section_function_body,
                               name, &len);
  gcc_assert (data);

  /* Do a bit copy of the function body.  */
  lto_output_data_stream (output_stream, data, len);
  lto_write_stream (output_stream);

  /* Copy decls. */
  in_state =
    lto_get_function_in_decl_state (node->lto_file_data, function);
  gcc_assert (in_state);

  for (i = 0; i < LTO_N_DECL_STREAMS; i++)
    {
      size_t n = in_state->streams[i].size;
      tree *trees = in_state->streams[i].trees;
      struct lto_tree_ref_encoder *encoder = &(out_state->streams[i]);

      /* The out state must have the same indices and the in state.
	 So just copy the vector.  All the encoders in the in state
	 must be empty where we reach here. */
      gcc_assert (lto_tree_ref_encoder_size (encoder) == 0);
      encoder->trees.reserve_exact (n);
      for (j = 0; j < n; j++)
	encoder->trees.safe_push (trees[j]);
    }

  lto_free_section_data (file_data, LTO_section_function_body, name,
			 data, len);
  free (output_stream);
  lto_end_section ();
}

/* Wrap symbol references in *TP inside a type-preserving MEM_REF.  */

static tree
wrap_refs (tree *tp, int *ws, void *)
{
  tree t = *tp;
  if (handled_component_p (t)
      && TREE_CODE (TREE_OPERAND (t, 0)) == VAR_DECL)
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
#ifdef ENABLE_CHECKING
  bitmap output = lto_bitmap_alloc ();
#endif
  int i, n_nodes;
  lto_symtab_encoder_t encoder = lto_get_out_decl_state ()->symtab_node_encoder;

  /* Initialize the streamer.  */
  lto_streamer_init ();

  n_nodes = lto_symtab_encoder_size (encoder);
  /* Process only the functions with bodies.  */
  for (i = 0; i < n_nodes; i++)
    {
      symtab_node *snode = lto_symtab_encoder_deref (encoder, i);
      if (cgraph_node *node = dyn_cast <cgraph_node> (snode))
	{
	  if (lto_symtab_encoder_encode_body_p (encoder, node)
	      && !node->alias)
	    {
#ifdef ENABLE_CHECKING
	      gcc_assert (!bitmap_bit_p (output, DECL_UID (node->decl)));
	      bitmap_set_bit (output, DECL_UID (node->decl));
#endif
	      decl_state = lto_new_out_decl_state ();
	      lto_push_out_decl_state (decl_state);
	      if (gimple_has_body_p (node->decl) || !flag_wpa)
		output_function (node);
	      else
		copy_function (node);
	      gcc_assert (lto_get_out_decl_state () == decl_state);
	      lto_pop_out_decl_state ();
	      lto_record_function_out_decl_state (node->decl, decl_state);
	    }
	}
      else if (varpool_node *node = dyn_cast <varpool_node> (snode))
	{
	  /* Wrap symbol references inside the ctor in a type
	     preserving MEM_REF.  */
	  tree ctor = DECL_INITIAL (node->decl);
	  if (ctor && !in_lto_p)
	    walk_tree (&ctor, wrap_refs, NULL, NULL);
	}
    }

  /* Emit the callgraph after emitting function bodies.  This needs to
     be done now to make sure that all the statements in every function
     have been renumbered so that edges can be associated with call
     statements using the statement UIDs.  */
  output_symtab ();

#ifdef ENABLE_CHECKING
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
			 struct lto_output_stream *ref_stream,
 			 struct lto_tree_ref_encoder *encoder)
{
  tree t;
  uint32_t index;
  const uint32_t size = lto_tree_ref_encoder_size (encoder);

  /* Write size as 32-bit unsigned. */
  lto_output_data_stream (ref_stream, &size, sizeof (int32_t));

  for (index = 0; index < size; index++)
    {
      uint32_t slot_num;

      t = lto_tree_ref_encoder_get_tree (encoder, index);
      streamer_tree_cache_lookup (ob->writer_cache, t, &slot_num);
      gcc_assert (slot_num != (unsigned)-1);
      lto_output_data_stream (ref_stream, &slot_num, sizeof slot_num);
    }
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
			    struct lto_output_stream *out_stream,
			    struct lto_out_decl_state *state)
{
  unsigned i;
  uint32_t ref;
  tree decl;

  /* Write reference to FUNCTION_DECL.  If there is not function,
     write reference to void_type_node. */
  decl = (state->fn_decl) ? state->fn_decl : void_type_node;
  streamer_tree_cache_lookup (ob->writer_cache, decl, &ref);
  gcc_assert (ref != (unsigned)-1);
  lto_output_data_stream (out_stream, &ref, sizeof (uint32_t));

  for (i = 0;  i < LTO_N_DECL_STREAMS; i++)
    write_global_references (ob, out_stream, &state->streams[i]);
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
	      struct lto_output_stream *stream,
	      tree t, struct pointer_set_t *seen, bool alias)
{
  const char *name;
  enum gcc_plugin_symbol_kind kind;
  enum gcc_plugin_symbol_visibility visibility;
  unsigned slot_num;
  unsigned HOST_WIDEST_INT size;
  const char *comdat;
  unsigned char c;

  /* None of the following kinds of symbols are needed in the
     symbol table.  */
  if (!TREE_PUBLIC (t)
      || is_builtin_fn (t)
      || DECL_ABSTRACT (t)
      || (TREE_CODE (t) == VAR_DECL && DECL_HARD_REGISTER (t)))
    return;
  gcc_assert (TREE_CODE (t) != RESULT_DECL);

  gcc_assert (TREE_CODE (t) == VAR_DECL
	      || TREE_CODE (t) == FUNCTION_DECL);

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (t));

  /* This behaves like assemble_name_raw in varasm.c, performing the
     same name manipulations that ASM_OUTPUT_LABELREF does. */
  name = IDENTIFIER_POINTER ((*targetm.asm_out.mangle_assembler_name) (name));

  if (pointer_set_contains (seen, name))
    return;
  pointer_set_insert (seen, name);

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
      gcc_assert (alias || TREE_CODE (t) != VAR_DECL
		  || varpool_get_node (t)->definition);
      gcc_assert (alias || TREE_CODE (t) != FUNCTION_DECL
		  || (cgraph_get_node (t)
		      && cgraph_get_node (t)->definition));
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
    comdat = IDENTIFIER_POINTER (DECL_COMDAT_GROUP (t));
  else
    comdat = "";

  lto_output_data_stream (stream, name, strlen (name) + 1);
  lto_output_data_stream (stream, comdat, strlen (comdat) + 1);
  c = (unsigned char) kind;
  lto_output_data_stream (stream, &c, 1);
  c = (unsigned char) visibility;
  lto_output_data_stream (stream, &c, 1);
  lto_output_data_stream (stream, &size, 8);
  lto_output_data_stream (stream, &slot_num, 4);
}

/* Return true if NODE should appear in the plugin symbol table.  */

bool
output_symbol_p (symtab_node *node)
{
  struct cgraph_node *cnode;
  if (!symtab_real_symbol_p (node))
    return false;
  /* We keep external functions in symtab for sake of inlining
     and devirtualization.  We do not want to see them in symbol table as
     references unless they are really used.  */
  cnode = dyn_cast <cgraph_node> (node);
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
      for (i = 0; ipa_ref_list_referring_iterate (&node->ref_list,
					          i, ref); i++)
	{
	  if (ref->use == IPA_REF_ALIAS)
	    continue;
          if (is_a <cgraph_node> (ref->referring))
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
  struct pointer_set_t *seen;
  struct lto_output_stream stream;
  lto_symtab_encoder_t encoder = ob->decl_state->symtab_node_encoder;
  lto_symtab_encoder_iterator lsei;

  lto_begin_section (section_name, false);
  free (section_name);

  seen = pointer_set_create ();
  memset (&stream, 0, sizeof (stream));

  /* Write the symbol table.
     First write everything defined and then all declarations.
     This is necessary to handle cases where we have duplicated symbols.  */
  for (lsei = lsei_start (encoder);
       !lsei_end_p (lsei); lsei_next (&lsei))
    {
      symtab_node *node = lsei_node (lsei);

      if (!output_symbol_p (node) || DECL_EXTERNAL (node->decl))
	continue;
      write_symbol (cache, &stream, node->decl, seen, false);
    }
  for (lsei = lsei_start (encoder);
       !lsei_end_p (lsei); lsei_next (&lsei))
    {
      symtab_node *node = lsei_node (lsei);

      if (!output_symbol_p (node) || !DECL_EXTERNAL (node->decl))
	continue;
      write_symbol (cache, &stream, node->decl, seen, false);
    }

  lto_write_stream (&stream);
  pointer_set_destroy (seen);

  lto_end_section ();
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
  struct lto_output_stream *header_stream, *decl_state_stream;
  unsigned idx, num_fns;
  size_t decl_state_size;
  int32_t num_decl_states;

  ob = create_output_block (LTO_section_decls);
  ob->global = true;

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

  header.lto_header.major_version = LTO_major_version;
  header.lto_header.minor_version = LTO_minor_version;

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

  header_stream = XCNEW (struct lto_output_stream);
  lto_output_data_stream (header_stream, &header, sizeof header);
  lto_write_stream (header_stream);
  free (header_stream);

  /* Write the main out-decl state, followed by out-decl states of
     functions. */
  decl_state_stream = XCNEW (struct lto_output_stream);
  num_decl_states = num_fns + 1;
  lto_output_data_stream (decl_state_stream, &num_decl_states,
			  sizeof (num_decl_states));
  lto_output_decl_state_refs (ob, decl_state_stream, out_state);
  for (idx = 0; idx < num_fns; idx++)
    {
      fn_out_state =
	lto_function_decl_states[idx];
      lto_output_decl_state_refs (ob, decl_state_stream, fn_out_state);
    }
  lto_write_stream (decl_state_stream);
  free (decl_state_stream);

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
}
