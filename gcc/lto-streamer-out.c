/* Write the GIMPLE representation to a file stream.

   Copyright 2009 Free Software Foundation, Inc.
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
#include "toplev.h"
#include "tree.h"
#include "expr.h"
#include "flags.h"
#include "params.h"
#include "input.h"
#include "varray.h"
#include "hashtab.h"
#include "basic-block.h"
#include "tree-flow.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "function.h"
#include "ggc.h"
#include "diagnostic.h"
#include "except.h"
#include "vec.h"
#include "lto-symtab.h"
#include "lto-streamer.h"


struct string_slot
{
  const char *s;
  int len;
  unsigned int slot_num;
};


/* Returns a hash code for P.  */

static hashval_t
hash_string_slot_node (const void *p)
{
  const struct string_slot *ds = (const struct string_slot *) p;
  return (hashval_t) htab_hash_string (ds->s);
}


/* Returns nonzero if P1 and P2 are equal.  */

static int
eq_string_slot_node (const void *p1, const void *p2)
{
  const struct string_slot *ds1 = (const struct string_slot *) p1;
  const struct string_slot *ds2 = (const struct string_slot *) p2;

  if (ds1->len == ds2->len)
    {
      int i;
      for (i = 0; i < ds1->len; i++)
	if (ds1->s[i] != ds2->s[i])
	  return 0;
      return 1;
    }

  return 0;
}


/* Free the string slot pointed-to by P.  */

static void
string_slot_free (void *p)
{
  struct string_slot *slot = (struct string_slot *) p;
  free (CONST_CAST (void *, (const void *) slot->s));
  free (slot);
}


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
  ob->writer_cache = lto_streamer_cache_create ();

  if (section_type == LTO_section_function_body)
    ob->cfg_stream = XCNEW (struct lto_output_stream);

  clear_line_info (ob);

  ob->string_hash_table = htab_create (37, hash_string_slot_node,
				       eq_string_slot_node, string_slot_free);

  return ob;
}


/* Destroy the output block OB.  */

void
destroy_output_block (struct output_block *ob)
{
  enum lto_section_type section_type = ob->section_type;

  htab_delete (ob->string_hash_table);

  free (ob->main_stream);
  free (ob->string_stream);
  if (section_type == LTO_section_function_body)
    free (ob->cfg_stream);

  lto_streamer_cache_delete (ob->writer_cache);

  free (ob);
}


/* Output bitpack BP to output stream S.  */

void
lto_output_bitpack (struct lto_output_stream *s, struct bitpack_d *bp)
{
  unsigned i;
  bitpack_word_t v;

  lto_output_uleb128_stream (s, VEC_length (bitpack_word_t, bp->values));
  for (i = 0; VEC_iterate (bitpack_word_t, bp->values, i, v); i++)
    lto_output_uleb128_stream (s, v);
}


/* Output STRING of LEN characters to the string
   table in OB. The string might or might not include a trailing '\0'.
   Then put the index onto the INDEX_STREAM.  */

static void
output_string_with_length (struct output_block *ob,
			   struct lto_output_stream *index_stream,
			   const char *s,
			   unsigned int len)
{
  struct string_slot **slot;
  struct string_slot s_slot;
  char *string = (char *) xmalloc (len + 1);
  memcpy (string, s, len);
  string[len] = '\0';

  s_slot.s = string;
  s_slot.len = len;
  s_slot.slot_num = 0;

  slot = (struct string_slot **) htab_find_slot (ob->string_hash_table,
						 &s_slot, INSERT);
  if (*slot == NULL)
    {
      struct lto_output_stream *string_stream = ob->string_stream;
      unsigned int start = string_stream->total_size;
      struct string_slot *new_slot
	= (struct string_slot *) xmalloc (sizeof (struct string_slot));
      unsigned int i;

      new_slot->s = string;
      new_slot->len = len;
      new_slot->slot_num = start;
      *slot = new_slot;
      lto_output_uleb128_stream (index_stream, start);
      lto_output_uleb128_stream (string_stream, len);
      for (i = 0; i < len; i++)
	lto_output_1_stream (string_stream, string[i]);
    }
  else
    {
      struct string_slot *old_slot = (struct string_slot *)*slot;
      lto_output_uleb128_stream (index_stream, old_slot->slot_num);
      free (string);
    }
}

/* Output the '\0' terminated STRING to the string
   table in OB.  Then put the index onto the INDEX_STREAM.  */

static void
output_string (struct output_block *ob,
	       struct lto_output_stream *index_stream,
	       const char *string)
{
  if (string)
    {
      lto_output_uleb128_stream (index_stream, 0);
      output_string_with_length (ob, index_stream, string, strlen (string) + 1);
    }
  else
    lto_output_uleb128_stream (index_stream, 1);
}


/* Output the STRING constant to the string
   table in OB.  Then put the index onto the INDEX_STREAM.  */

static void
output_string_cst (struct output_block *ob,
		   struct lto_output_stream *index_stream,
		   tree string)
{
  if (string)
    {
      lto_output_uleb128_stream (index_stream, 0);
      output_string_with_length (ob, index_stream,
				 TREE_STRING_POINTER (string),
				 TREE_STRING_LENGTH (string));
    }
  else
    lto_output_uleb128_stream (index_stream, 1);
}


/* Output the identifier ID to the string
   table in OB.  Then put the index onto the INDEX_STREAM.  */

static void
output_identifier (struct output_block *ob,
		   struct lto_output_stream *index_stream,
		   tree id)
{
  if (id)
    {
      lto_output_uleb128_stream (index_stream, 0);
      output_string_with_length (ob, index_stream,
				 IDENTIFIER_POINTER (id),
				 IDENTIFIER_LENGTH (id));
    }
  else
    lto_output_uleb128_stream (index_stream, 1);
}

/* Write a zero to the output stream.  */

static void
output_zero (struct output_block *ob)
{
  lto_output_1_stream (ob->main_stream, 0);
}


/* Output an unsigned LEB128 quantity to OB->main_stream.  */

static void
output_uleb128 (struct output_block *ob, unsigned HOST_WIDE_INT work)
{
  lto_output_uleb128_stream (ob->main_stream, work);
}


/* Output a signed LEB128 quantity to OB->main_stream.  */

static void
output_sleb128 (struct output_block *ob, HOST_WIDE_INT work)
{
  lto_output_sleb128_stream (ob->main_stream, work);
}


/* Output the start of a record with TAG to output block OB.  */

static void
output_record_start (struct output_block *ob, enum LTO_tags tag)
{
  /* Make sure TAG fits inside an unsigned int.  */
  gcc_assert (tag == (enum LTO_tags) (unsigned) tag);
  output_uleb128 (ob, tag);
}


/* Look up NODE in the type table and write the index for it to OB.  */

static void
output_type_ref (struct output_block *ob, tree node)
{
  output_record_start (ob, LTO_type_ref);
  lto_output_type_ref_index (ob->decl_state, ob->main_stream, node);
}


/* Pack all the non-pointer fields of the TS_BASE structure of
   expression EXPR into bitpack BP.  */

static void
pack_ts_base_value_fields (struct bitpack_d *bp, tree expr)
{
  bp_pack_value (bp, TREE_CODE (expr), 16);
  if (!TYPE_P (expr))
    {
      bp_pack_value (bp, TREE_SIDE_EFFECTS (expr), 1);
      bp_pack_value (bp, TREE_CONSTANT (expr), 1);
      bp_pack_value (bp, TREE_READONLY (expr), 1);

      /* TREE_PUBLIC is used on types to indicate that the type
	 has a TYPE_CACHED_VALUES vector.  This is not streamed out,
	 so we skip it here.  */
      bp_pack_value (bp, TREE_PUBLIC (expr), 1);
    }
  bp_pack_value (bp, TREE_ADDRESSABLE (expr), 1);
  bp_pack_value (bp, TREE_THIS_VOLATILE (expr), 1);
  if (DECL_P (expr))
    bp_pack_value (bp, DECL_UNSIGNED (expr), 1);
  else if (TYPE_P (expr))
    bp_pack_value (bp, TYPE_UNSIGNED (expr), 1);
  /* We write debug info two times, do not confuse the second one.  */
  bp_pack_value (bp, TYPE_P (expr) ? 0 : TREE_ASM_WRITTEN (expr), 1);
  bp_pack_value (bp, TREE_NO_WARNING (expr), 1);
  bp_pack_value (bp, TREE_USED (expr), 1);
  bp_pack_value (bp, TREE_NOTHROW (expr), 1);
  bp_pack_value (bp, TREE_STATIC (expr), 1);
  bp_pack_value (bp, TREE_PRIVATE (expr), 1);
  bp_pack_value (bp, TREE_PROTECTED (expr), 1);
  bp_pack_value (bp, TREE_DEPRECATED (expr), 1);
  if (TYPE_P (expr))
    bp_pack_value (bp, TYPE_SATURATING (expr), 1);
  if (TREE_CODE (expr) == SSA_NAME)
    bp_pack_value (bp, SSA_NAME_IS_DEFAULT_DEF (expr), 1);
}


/* Pack all the non-pointer fields of the TS_REAL_CST structure of
   expression EXPR into bitpack BP.  */

static void
pack_ts_real_cst_value_fields (struct bitpack_d *bp, tree expr)
{
  unsigned i;
  REAL_VALUE_TYPE r;

  r = TREE_REAL_CST (expr);
  bp_pack_value (bp, r.cl, 2);
  bp_pack_value (bp, r.decimal, 1);
  bp_pack_value (bp, r.sign, 1);
  bp_pack_value (bp, r.signalling, 1);
  bp_pack_value (bp, r.canonical, 1);
  bp_pack_value (bp, r.uexp, EXP_BITS);
  for (i = 0; i < SIGSZ; i++)
    bp_pack_value (bp, r.sig[i], HOST_BITS_PER_LONG);
}


/* Pack all the non-pointer fields of the TS_FIXED_CST structure of
   expression EXPR into bitpack BP.  */

static void
pack_ts_fixed_cst_value_fields (struct bitpack_d *bp, tree expr)
{
  struct fixed_value fv = TREE_FIXED_CST (expr);
  bp_pack_value (bp, fv.data.low, HOST_BITS_PER_WIDE_INT);
  bp_pack_value (bp, fv.data.high, HOST_BITS_PER_WIDE_INT);
}


/* Pack all the non-pointer fields of the TS_DECL_COMMON structure
   of expression EXPR into bitpack BP.  */

static void
pack_ts_decl_common_value_fields (struct bitpack_d *bp, tree expr)
{
  bp_pack_value (bp, DECL_MODE (expr), 8);
  bp_pack_value (bp, DECL_NONLOCAL (expr), 1);
  bp_pack_value (bp, DECL_VIRTUAL_P (expr), 1);
  bp_pack_value (bp, DECL_IGNORED_P (expr), 1);
  bp_pack_value (bp, DECL_ABSTRACT (expr), 1);
  bp_pack_value (bp, DECL_ARTIFICIAL (expr), 1);
  bp_pack_value (bp, DECL_USER_ALIGN (expr), 1);
  bp_pack_value (bp, DECL_PRESERVE_P (expr), 1);
  bp_pack_value (bp, DECL_DEBUG_EXPR_IS_FROM (expr), 1);
  bp_pack_value (bp, DECL_EXTERNAL (expr), 1);
  bp_pack_value (bp, DECL_GIMPLE_REG_P (expr), 1);
  bp_pack_value (bp, DECL_ALIGN (expr), HOST_BITS_PER_INT);

  if (TREE_CODE (expr) == LABEL_DECL)
    {
      /* Note that we do not write LABEL_DECL_UID.  The reader will
	 always assume an initial value of -1 so that the
	 label_to_block_map is recreated by gimple_set_bb.  */
      bp_pack_value (bp, DECL_ERROR_ISSUED (expr), 1);
      bp_pack_value (bp, EH_LANDING_PAD_NR (expr), HOST_BITS_PER_INT);
    }

  if (TREE_CODE (expr) == FIELD_DECL)
    {
      bp_pack_value (bp, DECL_PACKED (expr), 1);
      bp_pack_value (bp, DECL_NONADDRESSABLE_P (expr), 1);
      bp_pack_value (bp, DECL_OFFSET_ALIGN (expr), 8);
    }

  if (TREE_CODE (expr) == RESULT_DECL
      || TREE_CODE (expr) == PARM_DECL
      || TREE_CODE (expr) == VAR_DECL)
    {
      bp_pack_value (bp, DECL_BY_REFERENCE (expr), 1);
      if (TREE_CODE (expr) == VAR_DECL
	  || TREE_CODE (expr) == PARM_DECL)
	bp_pack_value (bp, DECL_HAS_VALUE_EXPR_P (expr), 1);
      bp_pack_value (bp, DECL_RESTRICTED_P (expr), 1);
    }
}


/* Pack all the non-pointer fields of the TS_DECL_WRTL structure
   of expression EXPR into bitpack BP.  */

static void
pack_ts_decl_wrtl_value_fields (struct bitpack_d *bp, tree expr)
{
  bp_pack_value (bp, DECL_REGISTER (expr), 1);
}


/* Pack all the non-pointer fields of the TS_DECL_WITH_VIS structure
   of expression EXPR into bitpack BP.  */

static void
pack_ts_decl_with_vis_value_fields (struct bitpack_d *bp, tree expr)
{
  bp_pack_value (bp, DECL_DEFER_OUTPUT (expr), 1);
  bp_pack_value (bp, DECL_COMMON (expr), 1);
  bp_pack_value (bp, DECL_DLLIMPORT_P (expr), 1);
  bp_pack_value (bp, DECL_WEAK (expr), 1);
  bp_pack_value (bp, DECL_SEEN_IN_BIND_EXPR_P (expr),  1);
  bp_pack_value (bp, DECL_COMDAT (expr),  1);
  bp_pack_value (bp, DECL_VISIBILITY (expr),  2);
  bp_pack_value (bp, DECL_VISIBILITY_SPECIFIED (expr),  1);

  if (TREE_CODE (expr) == VAR_DECL)
    {
      bp_pack_value (bp, DECL_HARD_REGISTER (expr), 1);
      bp_pack_value (bp, DECL_IN_TEXT_SECTION (expr), 1);
      bp_pack_value (bp, DECL_TLS_MODEL (expr),  3);
    }

  if (VAR_OR_FUNCTION_DECL_P (expr))
    bp_pack_value (bp, DECL_INIT_PRIORITY (expr), HOST_BITS_PER_SHORT);
}


/* Pack all the non-pointer fields of the TS_FUNCTION_DECL structure
   of expression EXPR into bitpack BP.  */

static void
pack_ts_function_decl_value_fields (struct bitpack_d *bp, tree expr)
{
  /* For normal/md builtins we only write the class and code, so they
     should never be handled here.  */
  gcc_assert (!lto_stream_as_builtin_p (expr));

  bp_pack_value (bp, DECL_FUNCTION_CODE (expr), 11);
  bp_pack_value (bp, DECL_BUILT_IN_CLASS (expr), 2);
  bp_pack_value (bp, DECL_STATIC_CONSTRUCTOR (expr), 1);
  bp_pack_value (bp, DECL_STATIC_DESTRUCTOR (expr), 1);
  bp_pack_value (bp, DECL_UNINLINABLE (expr), 1);
  bp_pack_value (bp, DECL_POSSIBLY_INLINED (expr), 1);
  bp_pack_value (bp, DECL_IS_NOVOPS (expr), 1);
  bp_pack_value (bp, DECL_IS_RETURNS_TWICE (expr), 1);
  bp_pack_value (bp, DECL_IS_MALLOC (expr), 1);
  bp_pack_value (bp, DECL_IS_OPERATOR_NEW (expr), 1);
  bp_pack_value (bp, DECL_DECLARED_INLINE_P (expr), 1);
  bp_pack_value (bp, DECL_STATIC_CHAIN (expr), 1);
  bp_pack_value (bp, DECL_NO_INLINE_WARNING_P (expr), 1);
  bp_pack_value (bp, DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (expr), 1);
  bp_pack_value (bp, DECL_NO_LIMIT_STACK (expr), 1);
  bp_pack_value (bp, DECL_DISREGARD_INLINE_LIMITS (expr), 1);
  bp_pack_value (bp, DECL_PURE_P (expr), 1);
  bp_pack_value (bp, DECL_LOOPING_CONST_OR_PURE_P (expr), 1);
}


/* Pack all the non-pointer fields of the TS_TYPE structure
   of expression EXPR into bitpack BP.  */

static void
pack_ts_type_value_fields (struct bitpack_d *bp, tree expr)
{
  bp_pack_value (bp, TYPE_PRECISION (expr), 9);
  bp_pack_value (bp, TYPE_MODE (expr), 7);
  bp_pack_value (bp, TYPE_STRING_FLAG (expr), 1);
  bp_pack_value (bp, TYPE_NO_FORCE_BLK (expr), 1);
  bp_pack_value (bp, TYPE_NEEDS_CONSTRUCTING(expr), 1);
  if (TREE_CODE (expr) == UNION_TYPE)
    bp_pack_value (bp, TYPE_TRANSPARENT_UNION (expr), 1);
  bp_pack_value (bp, TYPE_PACKED (expr), 1);
  bp_pack_value (bp, TYPE_RESTRICT (expr), 1);
  bp_pack_value (bp, TYPE_CONTAINS_PLACEHOLDER_INTERNAL (expr), 2);
  bp_pack_value (bp, TYPE_USER_ALIGN (expr), 1);
  bp_pack_value (bp, TYPE_READONLY (expr), 1);
  bp_pack_value (bp, TYPE_ALIGN (expr), HOST_BITS_PER_INT);
  bp_pack_value (bp, TYPE_ALIAS_SET (expr) == 0 ? 0 : -1, HOST_BITS_PER_INT);
}


/* Pack all the non-pointer fields of the TS_BLOCK structure
   of expression EXPR into bitpack BP.  */

static void
pack_ts_block_value_fields (struct bitpack_d *bp, tree expr)
{
  bp_pack_value (bp, BLOCK_ABSTRACT (expr), 1);
  bp_pack_value (bp, BLOCK_NUMBER (expr), 31);
}


/* Pack all the non-pointer fields in EXPR into a bit pack.  */

static struct bitpack_d *
pack_value_fields (tree expr)
{
  enum tree_code code;
  struct bitpack_d *bp;

  code = TREE_CODE (expr);
  bp = bitpack_create ();

  /* Note that all these functions are highly sensitive to changes in
     the types and sizes of each of the fields being packed.  */
  pack_ts_base_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_REAL_CST))
    pack_ts_real_cst_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_FIXED_CST))
    pack_ts_fixed_cst_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    pack_ts_decl_common_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WRTL))
    pack_ts_decl_wrtl_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    pack_ts_decl_with_vis_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    pack_ts_function_decl_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE))
    pack_ts_type_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_BLOCK))
    pack_ts_block_value_fields (bp, expr);

  if (CODE_CONTAINS_STRUCT (code, TS_SSA_NAME))
    {
      /* We only stream the version number of SSA names.  */
      gcc_unreachable ();
    }

  if (CODE_CONTAINS_STRUCT (code, TS_STATEMENT_LIST))
    {
      /* This is only used by GENERIC.  */
      gcc_unreachable ();
    }

  if (CODE_CONTAINS_STRUCT (code, TS_OMP_CLAUSE))
    {
      /* This is only used by High GIMPLE.  */
      gcc_unreachable ();
    }

  return bp;
}


/* Emit location LOC to output block OB.  */

static void
lto_output_location (struct output_block *ob, location_t loc)
{
  expanded_location xloc;

  if (loc == UNKNOWN_LOCATION)
    {
      output_string (ob, ob->main_stream, NULL);
      return;
    }

  xloc = expand_location (loc);

  output_string (ob, ob->main_stream, xloc.file);
  output_sleb128 (ob, xloc.line);
  output_sleb128 (ob, xloc.column);
  output_sleb128 (ob, xloc.sysp);

  ob->current_file = xloc.file;
  ob->current_line = xloc.line;
  ob->current_col = xloc.column;
}


/* Return true if tree node T is written to various tables.  For these
   nodes, we sometimes want to write their phyiscal representation
   (via lto_output_tree), and sometimes we need to emit an index
   reference into a table (via lto_output_tree_ref).  */

static bool
tree_is_indexable (tree t)
{
  if (TREE_CODE (t) == PARM_DECL)
    return false;
  else if (TREE_CODE (t) == VAR_DECL && decl_function_context (t)
	   && !TREE_STATIC (t))
    return false;
  else
    return (TYPE_P (t) || DECL_P (t) || TREE_CODE (t) == SSA_NAME);
}


/* If EXPR is an indexable tree node, output a reference to it to
   output block OB.  Otherwise, output the physical representation of
   EXPR to OB.  */

static void
lto_output_tree_ref (struct output_block *ob, tree expr)
{
  enum tree_code code;

  if (expr == NULL_TREE)
    {
      output_zero (ob);
      return;
    }

  if (!tree_is_indexable (expr))
    {
      /* Even though we are emitting the physical representation of
	 EXPR, its leaves must be emitted as references.  */
      lto_output_tree (ob, expr, true);
      return;
    }

  if (TYPE_P (expr))
    {
      output_type_ref (ob, expr);
      return;
    }

  code = TREE_CODE (expr);
  switch (code)
    {
    case SSA_NAME:
      output_record_start (ob, LTO_ssa_name_ref);
      output_uleb128 (ob, SSA_NAME_VERSION (expr));
      break;

    case FIELD_DECL:
      output_record_start (ob, LTO_field_decl_ref);
      lto_output_field_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case FUNCTION_DECL:
      output_record_start (ob, LTO_function_decl_ref);
      lto_output_fn_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case VAR_DECL:
    case DEBUG_EXPR_DECL:
      gcc_assert (decl_function_context (expr) == NULL
		  || TREE_STATIC (expr));
      output_record_start (ob, LTO_global_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case CONST_DECL:
      output_record_start (ob, LTO_const_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case IMPORTED_DECL:
      gcc_assert (decl_function_context (expr) == NULL);
      output_record_start (ob, LTO_imported_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case TYPE_DECL:
      output_record_start (ob, LTO_type_decl_ref);
      lto_output_type_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case NAMESPACE_DECL:
      output_record_start (ob, LTO_namespace_decl_ref);
      lto_output_namespace_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case LABEL_DECL:
      output_record_start (ob, LTO_label_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    case RESULT_DECL:
      output_record_start (ob, LTO_result_decl_ref);
      lto_output_var_decl_index (ob->decl_state, ob->main_stream, expr);
      break;

    default:
      /* No other node is indexable, so it should have been handled
	 by lto_output_tree.  */
      gcc_unreachable ();
    }
}


/* If REF_P is true, emit a reference to EXPR in output block OB,
   otherwise emit the physical representation of EXPR in OB.  */

static inline void
lto_output_tree_or_ref (struct output_block *ob, tree expr, bool ref_p)
{
  if (ref_p)
    lto_output_tree_ref (ob, expr);
  else
    lto_output_tree (ob, expr, false);
}


/* Emit the chain of tree nodes starting at T.  OB is the output block
   to write to.  REF_P is true if chain elements should be emitted
   as references.  */

static void
lto_output_chain (struct output_block *ob, tree t, bool ref_p)
{
  int i, count;

  count = list_length (t);
  output_sleb128 (ob, count);
  for (i = 0; i < count; i++)
    {
      tree saved_chain;

      /* Clear TREE_CHAIN to avoid blindly recursing into the rest
	 of the list.  */
      saved_chain = TREE_CHAIN (t);
      TREE_CHAIN (t) = NULL_TREE;

      lto_output_tree_or_ref (ob, t, ref_p);

      TREE_CHAIN (t) = saved_chain;
      t = TREE_CHAIN (t);
    }
}


/* Write all pointer fields in the TS_COMMON structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
lto_output_ts_common_tree_pointers (struct output_block *ob, tree expr,
				    bool ref_p)
{
  lto_output_tree_or_ref (ob, TREE_TYPE (expr), ref_p);
}


/* Write all pointer fields in the TS_VECTOR structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
lto_output_ts_vector_tree_pointers (struct output_block *ob, tree expr,
				    bool ref_p)
{
  lto_output_chain (ob, TREE_VECTOR_CST_ELTS (expr), ref_p);
}


/* Write all pointer fields in the TS_COMPLEX structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
lto_output_ts_complex_tree_pointers (struct output_block *ob, tree expr,
				     bool ref_p)
{
  lto_output_tree_or_ref (ob, TREE_REALPART (expr), ref_p);
  lto_output_tree_or_ref (ob, TREE_IMAGPART (expr), ref_p);
}


/* Write all pointer fields in the TS_DECL_MINIMAL structure of EXPR
   to output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
lto_output_ts_decl_minimal_tree_pointers (struct output_block *ob, tree expr,
					  bool ref_p)
{
  lto_output_tree_or_ref (ob, DECL_NAME (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_CONTEXT (expr), ref_p);
  lto_output_location (ob, DECL_SOURCE_LOCATION (expr));
}


/* Write all pointer fields in the TS_DECL_COMMON structure of EXPR to
   output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
lto_output_ts_decl_common_tree_pointers (struct output_block *ob, tree expr,
					 bool ref_p)
{
  lto_output_tree_or_ref (ob, DECL_SIZE (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_SIZE_UNIT (expr), ref_p);

  if (TREE_CODE (expr) != FUNCTION_DECL)
    lto_output_tree_or_ref (ob, DECL_INITIAL (expr), ref_p);

  lto_output_tree_or_ref (ob, DECL_ATTRIBUTES (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_ABSTRACT_ORIGIN (expr), ref_p);

  if (TREE_CODE (expr) == PARM_DECL)
    lto_output_chain (ob, TREE_CHAIN (expr), ref_p);

  if ((TREE_CODE (expr) == VAR_DECL
       || TREE_CODE (expr) == PARM_DECL)
      && DECL_HAS_VALUE_EXPR_P (expr))
    lto_output_tree_or_ref (ob, DECL_VALUE_EXPR (expr), ref_p);
}


/* Write all pointer fields in the TS_DECL_NON_COMMON structure of
   EXPR to output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
lto_output_ts_decl_non_common_tree_pointers (struct output_block *ob,
					     tree expr, bool ref_p)
{
  if (TREE_CODE (expr) == FUNCTION_DECL)
    {
      /* DECL_SAVED_TREE holds the GENERIC representation for DECL.
	 At this point, it should not exist.  Either because it was
	 converted to gimple or because DECL didn't have a GENERIC
	 representation in this TU.  */
      gcc_assert (DECL_SAVED_TREE (expr) == NULL_TREE);
      lto_output_tree_or_ref (ob, DECL_ARGUMENTS (expr), ref_p);
      lto_output_tree_or_ref (ob, DECL_RESULT (expr), ref_p);
    }
  lto_output_tree_or_ref (ob, DECL_VINDEX (expr), ref_p);
}


/* Write all pointer fields in the TS_DECL_WITH_VIS structure of EXPR
   to output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
lto_output_ts_decl_with_vis_tree_pointers (struct output_block *ob, tree expr,
					   bool ref_p)
{
  /* Make sure we don't inadvertently set the assembler name.  */
  if (DECL_ASSEMBLER_NAME_SET_P (expr))
    lto_output_tree_or_ref (ob, DECL_ASSEMBLER_NAME (expr), ref_p);
  else
    output_zero (ob);

  lto_output_tree_or_ref (ob, DECL_SECTION_NAME (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_COMDAT_GROUP (expr), ref_p);
}


/* Write all pointer fields in the TS_FIELD_DECL structure of EXPR to
   output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
lto_output_ts_field_decl_tree_pointers (struct output_block *ob, tree expr,
					bool ref_p)
{
  lto_output_tree_or_ref (ob, DECL_FIELD_OFFSET (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_BIT_FIELD_TYPE (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_QUALIFIER (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_FIELD_BIT_OFFSET (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_FCONTEXT (expr), ref_p);
  lto_output_chain (ob, TREE_CHAIN (expr), ref_p);
}


/* Write all pointer fields in the TS_FUNCTION_DECL structure of EXPR
   to output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
lto_output_ts_function_decl_tree_pointers (struct output_block *ob, tree expr,
					   bool ref_p)
{
  /* DECL_STRUCT_FUNCTION is handled by lto_output_function.  FIXME lto,
     maybe it should be handled here?  */
  lto_output_tree_or_ref (ob, DECL_FUNCTION_PERSONALITY (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_FUNCTION_SPECIFIC_TARGET (expr), ref_p);
  lto_output_tree_or_ref (ob, DECL_FUNCTION_SPECIFIC_OPTIMIZATION (expr),
			  ref_p);
}


/* Write all pointer fields in the TS_TYPE structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
lto_output_ts_type_tree_pointers (struct output_block *ob, tree expr,
				  bool ref_p)
{
  if (TREE_CODE (expr) == ENUMERAL_TYPE)
    lto_output_tree_or_ref (ob, TYPE_VALUES (expr), ref_p);
  else if (TREE_CODE (expr) == ARRAY_TYPE)
    lto_output_tree_or_ref (ob, TYPE_DOMAIN (expr), ref_p);
  else if (TREE_CODE (expr) == RECORD_TYPE || TREE_CODE (expr) == UNION_TYPE)
    lto_output_tree_or_ref (ob, TYPE_FIELDS (expr), ref_p);
  else if (TREE_CODE (expr) == FUNCTION_TYPE || TREE_CODE (expr) == METHOD_TYPE)
    lto_output_tree_or_ref (ob, TYPE_ARG_TYPES (expr), ref_p);
  else if (TREE_CODE (expr) == VECTOR_TYPE)
    lto_output_tree_or_ref (ob, TYPE_DEBUG_REPRESENTATION_TYPE (expr), ref_p);

  lto_output_tree_or_ref (ob, TYPE_SIZE (expr), ref_p);
  lto_output_tree_or_ref (ob, TYPE_SIZE_UNIT (expr), ref_p);
  lto_output_tree_or_ref (ob, TYPE_ATTRIBUTES (expr), ref_p);
  lto_output_tree_or_ref (ob, TYPE_NAME (expr), ref_p);
  /* Do not stream TYPE_POINTER_TO or TYPE_REFERENCE_TO nor
     TYPE_NEXT_PTR_TO or TYPE_NEXT_REF_TO.  */
  if (!POINTER_TYPE_P (expr))
    lto_output_tree_or_ref (ob, TYPE_MINVAL (expr), ref_p);
  lto_output_tree_or_ref (ob, TYPE_MAXVAL (expr), ref_p);
  lto_output_tree_or_ref (ob, TYPE_MAIN_VARIANT (expr), ref_p);
  /* Do not stream TYPE_NEXT_VARIANT, we reconstruct the variant lists
     during fixup.  */
  if (TREE_CODE (expr) == RECORD_TYPE || TREE_CODE (expr) == UNION_TYPE)
    lto_output_tree_or_ref (ob, TYPE_BINFO (expr), ref_p);
  lto_output_tree_or_ref (ob, TYPE_CONTEXT (expr), ref_p);
  lto_output_tree_or_ref (ob, TYPE_CANONICAL (expr), ref_p);
  lto_output_tree_or_ref (ob, TYPE_STUB_DECL (expr), ref_p);
}


/* Write all pointer fields in the TS_LIST structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
lto_output_ts_list_tree_pointers (struct output_block *ob, tree expr,
				  bool ref_p)
{
  lto_output_tree_or_ref (ob, TREE_PURPOSE (expr), ref_p);
  lto_output_tree_or_ref (ob, TREE_VALUE (expr), ref_p);
  lto_output_chain (ob, TREE_CHAIN (expr), ref_p);
}


/* Write all pointer fields in the TS_VEC structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
lto_output_ts_vec_tree_pointers (struct output_block *ob, tree expr, bool ref_p)
{
  int i;

  /* Note that the number of slots for EXPR has already been emitted
     in EXPR's header (see lto_output_tree_header).  */
  for (i = 0; i < TREE_VEC_LENGTH (expr); i++)
    lto_output_tree_or_ref (ob, TREE_VEC_ELT (expr, i), ref_p);
}


/* Write all pointer fields in the TS_EXP structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
lto_output_ts_exp_tree_pointers (struct output_block *ob, tree expr, bool ref_p)
{
  int i;

  output_sleb128 (ob, TREE_OPERAND_LENGTH (expr));
  for (i = 0; i < TREE_OPERAND_LENGTH (expr); i++)
    lto_output_tree_or_ref (ob, TREE_OPERAND (expr, i), ref_p);
  lto_output_location (ob, EXPR_LOCATION (expr));
  lto_output_tree_or_ref (ob, TREE_BLOCK (expr), ref_p);
}


/* Write all pointer fields in the TS_BLOCK structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
lto_output_ts_block_tree_pointers (struct output_block *ob, tree expr,
				   bool ref_p)
{
  unsigned i;
  tree t;

  lto_output_location (ob, BLOCK_SOURCE_LOCATION (expr));
  lto_output_chain (ob, BLOCK_VARS (expr), ref_p);

  output_uleb128 (ob, VEC_length (tree, BLOCK_NONLOCALIZED_VARS (expr)));
  for (i = 0; VEC_iterate (tree, BLOCK_NONLOCALIZED_VARS (expr), i, t); i++)
    lto_output_tree_or_ref (ob, t, ref_p);

  lto_output_tree_or_ref (ob, BLOCK_SUPERCONTEXT (expr), ref_p);
  lto_output_tree_or_ref (ob, BLOCK_ABSTRACT_ORIGIN (expr), ref_p);
  lto_output_tree_or_ref (ob, BLOCK_FRAGMENT_ORIGIN (expr), ref_p);
  lto_output_tree_or_ref (ob, BLOCK_FRAGMENT_CHAIN (expr), ref_p);
  lto_output_chain (ob, BLOCK_SUBBLOCKS (expr), ref_p);
}


/* Write all pointer fields in the TS_BINFO structure of EXPR to output
   block OB.  If REF_P is true, write a reference to EXPR's pointer
   fields.  */

static void
lto_output_ts_binfo_tree_pointers (struct output_block *ob, tree expr,
				   bool ref_p)
{
  unsigned i;
  tree t;

  /* Note that the number of BINFO slots has already been emitted in
     EXPR's header (see lto_output_tree_header) because this length
     is needed to build the empty BINFO node on the reader side.  */
  for (i = 0; VEC_iterate (tree, BINFO_BASE_BINFOS (expr), i, t); i++)
    lto_output_tree_or_ref (ob, t, ref_p);
  output_zero (ob);

  lto_output_tree_or_ref (ob, BINFO_OFFSET (expr), ref_p);
  lto_output_tree_or_ref (ob, BINFO_VTABLE (expr), ref_p);
  lto_output_tree_or_ref (ob, BINFO_VIRTUALS (expr), ref_p);
  lto_output_tree_or_ref (ob, BINFO_VPTR_FIELD (expr), ref_p);

  output_uleb128 (ob, VEC_length (tree, BINFO_BASE_ACCESSES (expr)));
  for (i = 0; VEC_iterate (tree, BINFO_BASE_ACCESSES (expr), i, t); i++)
    lto_output_tree_or_ref (ob, t, ref_p);

  lto_output_tree_or_ref (ob, BINFO_INHERITANCE_CHAIN (expr), ref_p);
  lto_output_tree_or_ref (ob, BINFO_SUBVTT_INDEX (expr), ref_p);
  lto_output_tree_or_ref (ob, BINFO_VPTR_INDEX (expr), ref_p);
}


/* Write all pointer fields in the TS_CONSTRUCTOR structure of EXPR to
   output block OB.  If REF_P is true, write a reference to EXPR's
   pointer fields.  */

static void
lto_output_ts_constructor_tree_pointers (struct output_block *ob, tree expr,
					 bool ref_p)
{
  unsigned i;
  tree index, value;

  output_uleb128 (ob, CONSTRUCTOR_NELTS (expr));
  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (expr), i, index, value)
    {
      lto_output_tree_or_ref (ob, index, ref_p);
      lto_output_tree_or_ref (ob, value, ref_p);
    }
}


/* Helper for lto_output_tree.  Write all pointer fields in EXPR to output
   block OB.  If REF_P is true, the leaves of EXPR are emitted as
   references.  */

static void
lto_output_tree_pointers (struct output_block *ob, tree expr, bool ref_p)
{
  enum tree_code code;

  code = TREE_CODE (expr);

  if (CODE_CONTAINS_STRUCT (code, TS_COMMON))
    lto_output_ts_common_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_VECTOR))
    lto_output_ts_vector_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_COMPLEX))
    lto_output_ts_complex_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_MINIMAL))
    lto_output_ts_decl_minimal_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_COMMON))
    lto_output_ts_decl_common_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_NON_COMMON))
    lto_output_ts_decl_non_common_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_DECL_WITH_VIS))
    lto_output_ts_decl_with_vis_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_FIELD_DECL))
    lto_output_ts_field_decl_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_FUNCTION_DECL))
    lto_output_ts_function_decl_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_TYPE))
    lto_output_ts_type_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_LIST))
    lto_output_ts_list_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_VEC))
    lto_output_ts_vec_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_EXP))
    lto_output_ts_exp_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_SSA_NAME))
    {
      /* We only stream the version number of SSA names.  */
      gcc_unreachable ();
    }

  if (CODE_CONTAINS_STRUCT (code, TS_BLOCK))
    lto_output_ts_block_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    lto_output_ts_binfo_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_CONSTRUCTOR))
    lto_output_ts_constructor_tree_pointers (ob, expr, ref_p);

  if (CODE_CONTAINS_STRUCT (code, TS_STATEMENT_LIST))
    {
      /* This should only appear in GENERIC.  */
      gcc_unreachable ();
    }

  if (CODE_CONTAINS_STRUCT (code, TS_OMP_CLAUSE))
    {
      /* This should only appear in High GIMPLE.  */
      gcc_unreachable ();
    }

  if (CODE_CONTAINS_STRUCT (code, TS_OPTIMIZATION))
    sorry ("gimple bytecode streams do not support the optimization attribute");

  if (CODE_CONTAINS_STRUCT (code, TS_TARGET_OPTION))
    sorry ("gimple bytecode streams do not support the target attribute");
}


/* Emit header information for tree EXPR to output block OB.  The header
   contains everything needed to instantiate an empty skeleton for
   EXPR on the reading side.  IX is the index into the streamer cache
   where EXPR is stored.  REF_P is as in lto_output_tree.  */

static void
lto_output_tree_header (struct output_block *ob, tree expr, int ix)
{
  enum LTO_tags tag;
  enum tree_code code;

  /* We should not see any non-GIMPLE tree nodes here.  */
  code = TREE_CODE (expr);
  if (!lto_is_streamable (expr))
    internal_error ("tree code %qs is not supported in gimple streams",
		    tree_code_name[code]);

  /* The header of a tree node consists of its tag, the size of
     the node, and any other information needed to instantiate
     EXPR on the reading side (such as the number of slots in
     variable sized nodes).  */
  tag = lto_tree_code_to_tag (code);
  output_record_start (ob, tag);
  output_sleb128 (ob, ix);

  /* The following will cause bootstrap miscomparisons.  Enable with care.  */
#ifdef LTO_STREAMER_DEBUG
  /* This is used mainly for debugging purposes.  When the reader
     and the writer do not agree on a streamed node, the pointer
     value for EXPR can be used to track down the differences in
     the debugger.  */
  gcc_assert ((HOST_WIDEST_INT) (intptr_t) expr == (intptr_t) expr);
  output_sleb128 (ob, (HOST_WIDEST_INT) (intptr_t) expr);
#endif

  /* The text in strings and identifiers are completely emitted in
     the header.  */
  if (CODE_CONTAINS_STRUCT (code, TS_STRING))
    output_string_cst (ob, ob->main_stream, expr);
  else if (CODE_CONTAINS_STRUCT (code, TS_IDENTIFIER))
    output_identifier (ob, ob->main_stream, expr);
  else if (CODE_CONTAINS_STRUCT (code, TS_VEC))
    output_sleb128 (ob, TREE_VEC_LENGTH (expr));
  else if (CODE_CONTAINS_STRUCT (code, TS_BINFO))
    output_uleb128 (ob, BINFO_N_BASE_BINFOS (expr));
}


/* Write the code and class of builtin EXPR to output block OB.  IX is
   the index into the streamer cache where EXPR is stored.*/

static void
lto_output_builtin_tree (struct output_block *ob, tree expr, int ix)
{
  gcc_assert (lto_stream_as_builtin_p (expr));

  if (DECL_BUILT_IN_CLASS (expr) == BUILT_IN_MD
      && !targetm.builtin_decl)
    sorry ("gimple bytecode streams do not support machine specific builtin "
	   "functions on this target");

  output_record_start (ob, LTO_builtin_decl);
  output_uleb128 (ob, DECL_BUILT_IN_CLASS (expr));
  output_uleb128 (ob, DECL_FUNCTION_CODE (expr));
  output_sleb128 (ob, ix);

  if (DECL_ASSEMBLER_NAME_SET_P (expr))
    {
      /* When the assembler name of a builtin gets a user name,
	 the new name is always prefixed with '*' by
	 set_builtin_user_assembler_name.  So, to prevent the
	 reader side from adding a second '*', we omit it here.  */
      const char *str = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (expr));
      if (strlen (str) > 1 && str[0] == '*')
	output_string (ob, ob->main_stream, &str[1]);
      else
	output_string (ob, ob->main_stream, NULL);
    }
  else
    output_string (ob, ob->main_stream, NULL);
}


/* Write a physical representation of tree node EXPR to output block
   OB.  If REF_P is true, the leaves of EXPR are emitted as references
   via lto_output_tree_ref.  IX is the index into the streamer cache
   where EXPR is stored.  */

static void
lto_write_tree (struct output_block *ob, tree expr, bool ref_p, int ix)
{
  struct bitpack_d *bp;

  /* Write the header, containing everything needed to materialize
     EXPR on the reading side.  */
  lto_output_tree_header (ob, expr, ix);

  /* Pack all the non-pointer fields in EXPR into a bitpack and write
     the resulting bitpack.  */
  bp = pack_value_fields (expr);
  lto_output_bitpack (ob->main_stream, bp);
  bitpack_delete (bp);

  /* Write all the pointer fields in EXPR.  */
  lto_output_tree_pointers (ob, expr, ref_p);

  /* Mark the end of EXPR.  */
  output_zero (ob);
}


/* Emit the integer constant CST to output block OB.  If REF_P is true,
   CST's type will be emitted as a reference.  */

static void
lto_output_integer_cst (struct output_block *ob, tree cst, bool ref_p)
{
  output_record_start (ob, lto_tree_code_to_tag (INTEGER_CST));
  lto_output_tree_or_ref (ob, TREE_TYPE (cst), ref_p);
  lto_output_1_stream (ob->main_stream, TREE_OVERFLOW_P (cst));
  output_uleb128 (ob, TREE_INT_CST_LOW (cst));
  output_uleb128 (ob, TREE_INT_CST_HIGH (cst));
}


/* Emit the physical representation of tree node EXPR to output block
   OB.  If REF_P is true, the leaves of EXPR are emitted as references
   via lto_output_tree_ref.  */

void
lto_output_tree (struct output_block *ob, tree expr, bool ref_p)
{
  int ix;
  bool existed_p;
  unsigned offset;

  if (expr == NULL_TREE)
    {
      output_zero (ob);
      return;
    }

  /* INTEGER_CST nodes are special because they need their original type
     to be materialized by the reader (to implement TYPE_CACHED_VALUES).  */
  if (TREE_CODE (expr) == INTEGER_CST)
    {
      lto_output_integer_cst (ob, expr, ref_p);
      return;
    }

  /* Determine the offset in the stream where EXPR will be written.
     This is used when emitting pickle references so the reader knows
     where to reconstruct the pickled object from.  This allows
     circular and forward references within the same stream.  */
  offset = ob->main_stream->total_size;

  existed_p = lto_streamer_cache_insert (ob->writer_cache, expr, &ix, &offset);
  if (existed_p)
    {
      /* If a node has already been streamed out, make sure that
	 we don't write it more than once.  Otherwise, the reader
	 will instantiate two different nodes for the same object.  */
      output_record_start (ob, LTO_tree_pickle_reference);
      output_sleb128 (ob, ix);
      output_uleb128 (ob, lto_tree_code_to_tag (TREE_CODE (expr)));
      output_uleb128 (ob, offset);
    }
  else if (lto_stream_as_builtin_p (expr))
    {
      /* MD and NORMAL builtins do not need to be written out
	 completely as they are always instantiated by the
	 compiler on startup.  The only builtins that need to
	 be written out are BUILT_IN_FRONTEND.  For all other
	 builtins, we simply write the class and code.  */
      lto_output_builtin_tree (ob, expr, ix);
    }
  else
    {
      /* This is the first time we see EXPR, write its fields
	 to OB.  */
      lto_write_tree (ob, expr, ref_p, ix);
    }
}


/* Output to OB a list of try/catch handlers starting with FIRST.  */

static void
output_eh_try_list (struct output_block *ob, eh_catch first)
{
  eh_catch n;

  for (n = first; n; n = n->next_catch)
    {
      output_record_start (ob, LTO_eh_catch);
      lto_output_tree_ref (ob, n->type_list);
      lto_output_tree_ref (ob, n->filter_list);
      lto_output_tree_ref (ob, n->label);
    }

  output_zero (ob);
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
      output_zero (ob);
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

  output_record_start (ob, tag);
  output_sleb128 (ob, r->index);

  if (r->outer)
    output_sleb128 (ob, r->outer->index);
  else
    output_zero (ob);

  if (r->inner)
    output_sleb128 (ob, r->inner->index);
  else
    output_zero (ob);

  if (r->next_peer)
    output_sleb128 (ob, r->next_peer->index);
  else
    output_zero (ob);

  if (r->type == ERT_TRY)
    {
      output_eh_try_list (ob, r->u.eh_try.first_catch);
    }
  else if (r->type == ERT_ALLOWED_EXCEPTIONS)
    {
      lto_output_tree_ref (ob, r->u.allowed.type_list);
      lto_output_tree_ref (ob, r->u.allowed.label);
      output_uleb128 (ob, r->u.allowed.filter);
    }
  else if (r->type == ERT_MUST_NOT_THROW)
    {
      lto_output_tree_ref (ob, r->u.must_not_throw.failure_decl);
      lto_output_location (ob, r->u.must_not_throw.failure_loc);
    }

  if (r->landing_pads)
    output_sleb128 (ob, r->landing_pads->index);
  else
    output_zero (ob);
}


/* Output landing pad LP to OB.  */

static void
output_eh_lp (struct output_block *ob, eh_landing_pad lp)
{
  if (lp == NULL)
    {
      output_zero (ob);
      return;
    }

  output_record_start (ob, LTO_eh_landing_pad);
  output_sleb128 (ob, lp->index);
  if (lp->next_lp)
    output_sleb128 (ob, lp->next_lp->index);
  else
    output_zero (ob);

  if (lp->region)
    output_sleb128 (ob, lp->region->index);
  else
    output_zero (ob);

  lto_output_tree_ref (ob, lp->post_landing_pad);
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

      output_record_start (ob, LTO_eh_table);

      /* Emit the index of the root of the EH region tree.  */
      output_sleb128 (ob, fn->eh->region_tree->index);

      /* Emit all the EH regions in the region array.  */
      output_sleb128 (ob, VEC_length (eh_region, fn->eh->region_array));
      for (i = 0; VEC_iterate (eh_region, fn->eh->region_array, i, eh); i++)
	output_eh_region (ob, eh);

      /* Emit all landing pads.  */
      output_sleb128 (ob, VEC_length (eh_landing_pad, fn->eh->lp_array));
      for (i = 0; VEC_iterate (eh_landing_pad, fn->eh->lp_array, i, lp); i++)
	output_eh_lp (ob, lp);

      /* Emit all the runtime type data.  */
      output_sleb128 (ob, VEC_length (tree, fn->eh->ttype_data));
      for (i = 0; VEC_iterate (tree, fn->eh->ttype_data, i, ttype); i++)
	lto_output_tree_ref (ob, ttype);

      /* Emit the table of action chains.  */
      if (targetm.arm_eabi_unwinder)
	{
	  tree t;
	  output_sleb128 (ob, VEC_length (tree, fn->eh->ehspec_data.arm_eabi));
	  for (i = 0;
	       VEC_iterate (tree, fn->eh->ehspec_data.arm_eabi, i, t);
	       i++)
	    lto_output_tree_ref (ob, t);
	}
      else
	{
	  uchar c;
	  output_sleb128 (ob, VEC_length (uchar, fn->eh->ehspec_data.other));
	  for (i = 0; VEC_iterate (uchar, fn->eh->ehspec_data.other, i, c); i++)
	    lto_output_1_stream (ob->main_stream, c);
	}
    }

  /* The 0 either terminates the record or indicates that there are no
     eh_records at all.  */
  output_zero (ob);
}


/* Output all of the active ssa names to the ssa_names stream.  */

static void
output_ssa_names (struct output_block *ob, struct function *fn)
{
  unsigned int i, len;

  len = VEC_length (tree, SSANAMES (fn));
  output_uleb128 (ob, len);

  for (i = 1; i < len; i++)
    {
      tree ptr = VEC_index (tree, SSANAMES (fn), i);

      if (ptr == NULL_TREE
	  || SSA_NAME_IN_FREE_LIST (ptr)
	  || !is_gimple_reg (ptr))
	continue;

      output_uleb128 (ob, i);
      lto_output_1_stream (ob->main_stream, SSA_NAME_IS_DEFAULT_DEF (ptr));
      lto_output_tree_ref (ob, SSA_NAME_VAR (ptr));
    }

  output_zero (ob);
}


/* Output the cfg.  */

static void
output_cfg (struct output_block *ob, struct function *fn)
{
  struct lto_output_stream *tmp_stream = ob->main_stream;
  basic_block bb;

  ob->main_stream = ob->cfg_stream;

  output_uleb128 (ob, profile_status_for_function (fn));

  /* Output the number of the highest basic block.  */
  output_uleb128 (ob, last_basic_block_for_function (fn));

  FOR_ALL_BB_FN (bb, fn)
    {
      edge_iterator ei;
      edge e;

      output_sleb128 (ob, bb->index);

      /* Output the successors and the edge flags.  */
      output_uleb128 (ob, EDGE_COUNT (bb->succs));
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  output_uleb128 (ob, e->dest->index);
	  output_sleb128 (ob, e->probability);
	  output_sleb128 (ob, e->count);
	  output_uleb128 (ob, e->flags);
	}
    }

  output_sleb128 (ob, -1);

  bb = ENTRY_BLOCK_PTR;
  while (bb->next_bb)
    {
      output_sleb128 (ob, bb->next_bb->index);
      bb = bb->next_bb;
    }

  output_sleb128 (ob, -1);

  ob->main_stream = tmp_stream;
}


/* Output PHI function PHI to the main stream in OB.  */

static void
output_phi (struct output_block *ob, gimple phi)
{
  unsigned i, len = gimple_phi_num_args (phi);

  output_record_start (ob, lto_gimple_code_to_tag (GIMPLE_PHI));
  output_uleb128 (ob, SSA_NAME_VERSION (PHI_RESULT (phi)));

  for (i = 0; i < len; i++)
    {
      lto_output_tree_ref (ob, gimple_phi_arg_def (phi, i));
      output_uleb128 (ob, gimple_phi_arg_edge (phi, i)->src->index);
      lto_output_location (ob, gimple_phi_arg_location (phi, i));
    }
}


/* Emit statement STMT on the main stream of output block OB.  */

static void
output_gimple_stmt (struct output_block *ob, gimple stmt)
{
  unsigned i;
  enum gimple_code code;
  enum LTO_tags tag;
  struct bitpack_d *bp;

  /* Emit identifying tag.  */
  code = gimple_code (stmt);
  tag = lto_gimple_code_to_tag (code);
  output_record_start (ob, tag);

  /* Emit the tuple header.  */
  bp = bitpack_create ();
  bp_pack_value (bp, gimple_num_ops (stmt), sizeof (unsigned) * 8);
  bp_pack_value (bp, gimple_no_warning_p (stmt), 1);
  if (is_gimple_assign (stmt))
    bp_pack_value (bp, gimple_assign_nontemporal_move_p (stmt), 1);
  bp_pack_value (bp, gimple_has_volatile_ops (stmt), 1);
  bp_pack_value (bp, stmt->gsbase.subcode, 16);
  lto_output_bitpack (ob->main_stream, bp);
  bitpack_delete (bp);

  /* Emit location information for the statement.  */
  lto_output_location (ob, gimple_location (stmt));

  /* Emit the lexical block holding STMT.  */
  lto_output_tree (ob, gimple_block (stmt), true);

  /* Emit the operands.  */
  switch (gimple_code (stmt))
    {
    case GIMPLE_RESX:
      output_sleb128 (ob, gimple_resx_region (stmt));
      break;

    case GIMPLE_EH_MUST_NOT_THROW:
      lto_output_tree_ref (ob, gimple_eh_must_not_throw_fndecl (stmt));
      break;

    case GIMPLE_EH_DISPATCH:
      output_sleb128 (ob, gimple_eh_dispatch_region (stmt));
      break;

    case GIMPLE_ASM:
      lto_output_uleb128_stream (ob->main_stream, gimple_asm_ninputs (stmt));
      lto_output_uleb128_stream (ob->main_stream, gimple_asm_noutputs (stmt));
      lto_output_uleb128_stream (ob->main_stream, gimple_asm_nclobbers (stmt));
      output_string (ob, ob->main_stream, gimple_asm_string (stmt));
      /* Fallthru  */

    case GIMPLE_ASSIGN:
    case GIMPLE_CALL:
    case GIMPLE_RETURN:
    case GIMPLE_SWITCH:
    case GIMPLE_LABEL:
    case GIMPLE_COND:
    case GIMPLE_GOTO:
    case GIMPLE_DEBUG:
      for (i = 0; i < gimple_num_ops (stmt); i++)
	{
	  tree op = gimple_op (stmt, i);
	  lto_output_tree_ref (ob, op);
	}
      break;

    case GIMPLE_NOP:
    case GIMPLE_PREDICT:
      break;

    default:
      gcc_unreachable ();
    }
}


/* Output a basic block BB to the main stream in OB for this FN.  */

static void
output_bb (struct output_block *ob, basic_block bb, struct function *fn)
{
  gimple_stmt_iterator bsi = gsi_start_bb (bb);

  output_record_start (ob,
		       (!gsi_end_p (bsi)) || phi_nodes (bb)
		        ? LTO_bb1
			: LTO_bb0);

  output_uleb128 (ob, bb->index);
  output_sleb128 (ob, bb->count);
  output_sleb128 (ob, bb->loop_depth);
  output_sleb128 (ob, bb->frequency);
  output_sleb128 (ob, bb->flags);

  if (!gsi_end_p (bsi) || phi_nodes (bb))
    {
      /* Output the statements.  The list of statements is terminated
	 with a zero.  */
      for (bsi = gsi_start_bb (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  int region;
	  gimple stmt = gsi_stmt (bsi);

	  output_gimple_stmt (ob, stmt);

	  /* Emit the EH region holding STMT.  */
	  region = lookup_stmt_eh_lp_fn (fn, stmt);
	  if (region != 0)
	    {
	      output_record_start (ob, LTO_eh_region);
	      output_sleb128 (ob, region);
	    }
	  else
	    output_zero (ob);
	}

      output_zero (ob);

      for (bsi = gsi_start_phis (bb); !gsi_end_p (bsi); gsi_next (&bsi))
	{
	  gimple phi = gsi_stmt (bsi);

	  /* Only emit PHIs for gimple registers.  PHI nodes for .MEM
	     will be filled in on reading when the SSA form is
	     updated.  */
	  if (is_gimple_reg (gimple_phi_result (phi)))
	    output_phi (ob, phi);
	}

      output_zero (ob);
    }
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
      section_name = lto_get_section_name (section_type, name);
    }
  else
    section_name = lto_get_section_name (section_type, NULL);

  lto_begin_section (section_name, !flag_wpa);
  free (section_name);

  /* The entire header is stream computed here.  */
  memset (&header, 0, sizeof (struct lto_function_header));

  /* Write the header.  */
  header.lto_header.major_version = LTO_major_version;
  header.lto_header.minor_version = LTO_minor_version;
  header.lto_header.section_type = section_type;

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


/* Output the body of function NODE->DECL.  */

static void
output_function (struct cgraph_node *node)
{
  struct bitpack_d *bp;
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
  current_function_decl = function;
  push_cfun (fn);

  /* Make string 0 be a NULL string.  */
  lto_output_1_stream (ob->string_stream, 0);

  output_record_start (ob, LTO_function);

  /* Write all the attributes for FN.  */
  bp = bitpack_create ();
  bp_pack_value (bp, fn->is_thunk, 1);
  bp_pack_value (bp, fn->has_local_explicit_reg_vars, 1);
  bp_pack_value (bp, fn->after_tree_profile, 1);
  bp_pack_value (bp, fn->returns_pcc_struct, 1);
  bp_pack_value (bp, fn->returns_struct, 1);
  bp_pack_value (bp, fn->always_inline_functions_inlined, 1);
  bp_pack_value (bp, fn->after_inlining, 1);
  bp_pack_value (bp, fn->dont_save_pending_sizes_p, 1);
  bp_pack_value (bp, fn->stdarg, 1);
  bp_pack_value (bp, fn->has_nonlocal_label, 1);
  bp_pack_value (bp, fn->calls_alloca, 1);
  bp_pack_value (bp, fn->calls_setjmp, 1);
  bp_pack_value (bp, fn->function_frequency, 2);
  bp_pack_value (bp, fn->va_list_fpr_size, 8);
  bp_pack_value (bp, fn->va_list_gpr_size, 8);
  lto_output_bitpack (ob->main_stream, bp);
  bitpack_delete (bp);

  /* Output the static chain and non-local goto save area.  */
  lto_output_tree_ref (ob, fn->static_chain_decl);
  lto_output_tree_ref (ob, fn->nonlocal_goto_save_area);

  /* Output all the local variables in the function.  */
  lto_output_tree_ref (ob, fn->local_decls);

  /* Output the head of the arguments list.  */
  lto_output_tree_ref (ob, DECL_ARGUMENTS (function));

  /* Output all the SSA names used in the function.  */
  output_ssa_names (ob, fn);

  /* Output any exception handling regions.  */
  output_eh_regions (ob, fn);

  /* Output DECL_INITIAL for the function, which contains the tree of
     lexical scopes.  */
  lto_output_tree (ob, DECL_INITIAL (function), true);

  /* We will renumber the statements.  The code that does this uses
     the same ordering that we use for serializing them so we can use
     the same code on the other end and not have to write out the
     statement numbers.  */
  renumber_gimple_stmt_uids ();

  /* Output the code for the function.  */
  FOR_ALL_BB_FN (bb, fn)
    output_bb (ob, bb, fn);

  /* The terminator for this function.  */
  output_zero (ob);

  output_cfg (ob, fn);

  /* Create a section to hold the pickled output of this function.   */
  produce_asm (ob, function);

  destroy_output_block (ob);

  current_function_decl = NULL;
  pop_cfun ();
}


/* Return true if alias pair P belongs to the set of cgraph nodes in
   SET.  If P is a an alias for a VAR_DECL, it can always be emitted.
   However, for FUNCTION_DECL aliases, we should only output the pair
   if it belongs to a function whose cgraph node is in SET.
   Otherwise, the LTRANS phase will get into trouble when finalizing
   aliases because the alias will refer to a function not defined in
   the file processed by LTRANS.  */

static bool
output_alias_pair_p (alias_pair *p, cgraph_node_set set)
{
  cgraph_node_set_iterator csi;
  struct cgraph_node *target_node;

  /* Always emit VAR_DECLs.  FIXME lto, we should probably only emit
     those VAR_DECLs that are instantiated in this file partition, but
     we have no easy way of knowing this based on SET.  */
  if (TREE_CODE (p->decl) == VAR_DECL)
    return true;

  /* Check if the assembler name for P->TARGET has its cgraph node in SET.  */
  gcc_assert (TREE_CODE (p->decl) == FUNCTION_DECL);
  target_node = cgraph_node_for_asm (p->target);
  csi = cgraph_node_set_find (set, target_node);
  return (!csi_end_p (csi));
}


/* Output any unreferenced global symbol defined in SET, alias pairs
   and labels.  */

static void
output_unreferenced_globals (cgraph_node_set set)
{
  struct output_block *ob;
  alias_pair *p;
  unsigned i;
  struct varpool_node *vnode;

  ob = create_output_block (LTO_section_static_initializer);
  ob->cgraph_node = NULL;

  clear_line_info (ob);

  /* Make string 0 be a NULL string.  */
  lto_output_1_stream (ob->string_stream, 0);

  /* Emit references for all the global symbols.  If a global symbol
     was never referenced in any of the functions of this file, it
     would not be emitted otherwise.  This will result in unreferenced
     symbols at link time if a file defines a global symbol but
     never references it.  */
  FOR_EACH_STATIC_VARIABLE (vnode)
    {
      tree var = vnode->decl;

      if (TREE_CODE (var) == VAR_DECL && TREE_PUBLIC (var))
	lto_output_tree_ref (ob, var);
    }

  output_zero (ob);

  /* Emit the alias pairs for the nodes in SET.  */
  for (i = 0; VEC_iterate (alias_pair, alias_pairs, i, p); i++)
    {
      if (output_alias_pair_p (p, set))
	{
	  lto_output_tree_ref (ob, p->decl);
	  lto_output_tree_ref (ob, p->target);
	}
    }

  output_zero (ob);

  produce_asm (ob, NULL);
  destroy_output_block (ob);
}


/* Copy the function body of NODE without deserializing. */

static void
copy_function (struct cgraph_node *node)
{
  tree function = node->decl;
  struct lto_file_decl_data *file_data = node->local.lto_file_data;
  struct lto_output_stream *output_stream = XCNEW (struct lto_output_stream);
  const char *data;
  size_t len;
  const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (function));
  char *section_name =
    lto_get_section_name (LTO_section_function_body, name);
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
    lto_get_function_in_decl_state (node->local.lto_file_data, function);
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
      for (j = 0; j < n; j++)
	VEC_safe_push (tree, heap, encoder->trees, trees[j]);
      encoder->next_index = n;
    }

  lto_free_section_data (file_data, LTO_section_function_body, name,
			 data, len);
  free (output_stream);
  lto_end_section ();
}


/* Initialize the LTO writer.  */

static void
lto_writer_init (void)
{
  lto_streamer_init ();
}


/* Main entry point from the pass manager.  */

static void
lto_output (cgraph_node_set set)
{
  struct cgraph_node *node;
  struct lto_out_decl_state *decl_state;
  cgraph_node_set_iterator csi;
  bitmap output = lto_bitmap_alloc ();

  lto_writer_init ();

  /* Process only the functions with bodies.  */
  for (csi = csi_start (set); !csi_end_p (csi); csi_next (&csi))
    {
      node = csi_node (csi);
      if (node->analyzed && !bitmap_bit_p (output, DECL_UID (node->decl)))
	{
	  bitmap_set_bit (output, DECL_UID (node->decl));
	  decl_state = lto_new_out_decl_state ();
	  lto_push_out_decl_state (decl_state);
	  if (!flag_wpa)
	    output_function (node);
	  else
	    copy_function (node);
	  gcc_assert (lto_get_out_decl_state () == decl_state);
	  lto_pop_out_decl_state ();
	  lto_record_function_out_decl_state (node->decl, decl_state);
	}
    }

  /* Emit the callgraph after emitting function bodies.  This needs to
     be done now to make sure that all the statements in every function
     have been renumbered so that edges can be associated with call
     statements using the statement UIDs.  */
  output_cgraph (set);

  lto_bitmap_free (output);
}

struct ipa_opt_pass_d pass_ipa_lto_gimple_out =
{
 {
  IPA_PASS,
  "lto_gimple_out",	                /* name */
  gate_lto_out,			        /* gate */
  NULL,		                	/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_IPA_LTO_GIMPLE_IO,		        /* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,            			/* todo_flags_start */
  TODO_dump_func                        /* todo_flags_finish */
 },
 NULL,		                        /* generate_summary */
 lto_output,           			/* write_summary */
 NULL,		         		/* read_summary */
 NULL,					/* function_read_summary */
 NULL,					/* stmt_fixup */
 0,					/* TODOs */
 NULL,			                /* function_transform */
 NULL					/* variable_transform */
};


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
      if (!lto_streamer_cache_lookup (ob->writer_cache, t, NULL))
	lto_output_tree (ob, t, false);

      if (flag_wpa)
	{
	  /* In WPA we should not emit multiple definitions of the
	     same symbol to all the files in the link set.  If
	     T had already been emitted as the pervailing definition
	     in one file, do not emit it in the others.  */
	  /* FIXME lto.  We should check if T belongs to the
	     file we are writing to.  */
	  if (TREE_CODE (t) == VAR_DECL
	      && TREE_PUBLIC (t)
	      && !DECL_EXTERNAL (t))
	    TREE_ASM_WRITTEN (t) = 1;
	}
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
  int32_t index;
  const int32_t size = lto_tree_ref_encoder_size (encoder);

  /* Write size as 32-bit unsigned. */
  lto_output_data_stream (ref_stream, &size, sizeof (int32_t));

  for (index = 0; index < size; index++)
    {
      int32_t slot_num;

      t = lto_tree_ref_encoder_get_tree (encoder, index);
      lto_streamer_cache_lookup (ob->writer_cache, t, &slot_num);
      gcc_assert (slot_num >= 0);
      lto_output_data_stream (ref_stream, &slot_num, sizeof slot_num);
    }
}


/* Write all the streams in an lto_out_decl_state STATE using
   output block OB and output stream OUT_STREAM.  */

static void
lto_output_decl_state_streams (struct output_block *ob,
			       struct lto_out_decl_state *state)
{
  int i;

  for (i = 0;  i < LTO_N_DECL_STREAMS; i++)
    write_global_stream (ob, &state->streams[i]);
}


/* Write all the references in an lto_out_decl_state STATE using
   output block OB and output stream OUT_STREAM.  */

static void
lto_output_decl_state_refs (struct output_block *ob,
			    struct lto_output_stream *out_stream,
			    struct lto_out_decl_state *state)
{
  unsigned i;
  int32_t ref;
  tree decl;

  /* Write reference to FUNCTION_DECL.  If there is not function,
     write reference to void_type_node. */
  decl = (state->fn_decl) ? state->fn_decl : void_type_node;
  lto_streamer_cache_lookup (ob->writer_cache, decl, &ref);
  gcc_assert (ref >= 0);
  lto_output_data_stream (out_stream, &ref, sizeof (int32_t));

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


/* Helper function of write_symbols_of_kind.  CACHE is the streamer
   cache with all the pickled nodes.  STREAM is the stream where to
   write the table.  V is a vector with the DECLs that should be on
   the table.  SEEN is a bitmap of symbols written so far.  */

static void
write_symbol_vec (struct lto_streamer_cache_d *cache,
		  struct lto_output_stream *stream,
		  VEC(tree,heap) *v, bitmap seen)
{
  tree t;
  int index;

  for (index = 0; VEC_iterate(tree, v, index, t); index++)
    {
      const char *name;
      enum gcc_plugin_symbol_kind kind;
      enum gcc_plugin_symbol_visibility visibility;
      int slot_num;
      uint64_t size;
      const char *comdat;

      /* None of the following kinds of symbols are needed in the
	 symbol table.  */
      if (!TREE_PUBLIC (t)
	  || is_builtin_fn (t)
	  || DECL_ABSTRACT (t)
	  || TREE_CODE (t) == RESULT_DECL)
	continue;

      gcc_assert (TREE_CODE (t) == VAR_DECL
		  || TREE_CODE (t) == FUNCTION_DECL);

      name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (t));

      /* FIXME lto: this is from assemble_name_raw in varasm.c. For some
	 architectures we might have to do the same name manipulations that
	 ASM_OUTPUT_LABELREF does. */
      if (name[0] == '*')
	name = &name[1];

      lto_streamer_cache_lookup (cache, t, &slot_num);
      gcc_assert (slot_num >= 0);

      /* Avoid duplicate symbols. */
      if (bitmap_bit_p (seen, slot_num))
	continue;
      else
        bitmap_set_bit (seen, slot_num);

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
	}

      switch (DECL_VISIBILITY(t))
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
	  && DECL_SIZE (t)
	  && TREE_CODE (DECL_SIZE (t)) == INTEGER_CST)
	size = (((uint64_t) TREE_INT_CST_HIGH (DECL_SIZE (t))) << 32)
	  | TREE_INT_CST_LOW (DECL_SIZE (t));
      else
	size = 0;

      if (DECL_ONE_ONLY (t))
	comdat = IDENTIFIER_POINTER (DECL_COMDAT_GROUP (t));
      else
	comdat = "";

      lto_output_data_stream (stream, name, strlen (name) + 1);
      lto_output_data_stream (stream, comdat, strlen (comdat) + 1);
      lto_output_data_stream (stream, &kind, 1);
      lto_output_data_stream (stream, &visibility, 1);
      lto_output_data_stream (stream, &size, 8);
      lto_output_data_stream (stream, &slot_num, 4);
    }
}


/* Write IL symbols of KIND.  CACHE is the streamer cache with all the
   pickled nodes.  SEEN is a bitmap of symbols written so far.  */

static void
write_symbols_of_kind (lto_decl_stream_e_t kind,
		       struct lto_streamer_cache_d *cache, bitmap seen)
{
  struct lto_out_decl_state *out_state;
  struct lto_output_stream stream;
  unsigned num_fns =
    VEC_length (lto_out_decl_state_ptr, lto_function_decl_states);
  unsigned idx;

  memset (&stream, 0, sizeof (stream));
  out_state = lto_get_out_decl_state ();
  write_symbol_vec (cache, &stream, out_state->streams[kind].trees, seen);

  for (idx = 0; idx < num_fns; idx++)
    {
      out_state =
	VEC_index (lto_out_decl_state_ptr, lto_function_decl_states, idx);
      write_symbol_vec (cache, &stream, out_state->streams[kind].trees, seen);
    }

  lto_write_stream (&stream);
}


/* Write an IL symbol table.  CACHE is the streamer cache with all the
   pickled nodes.  */

static void
produce_symtab (struct lto_streamer_cache_d *cache)
{
  char *section_name = lto_get_section_name (LTO_section_symtab, NULL);
  bitmap seen;

  lto_begin_section (section_name, false);
  free (section_name);

  seen = lto_bitmap_alloc ();
  write_symbols_of_kind (LTO_DECL_STREAM_FN_DECL, cache, seen);
  write_symbols_of_kind (LTO_DECL_STREAM_VAR_DECL, cache, seen);
  lto_bitmap_free (seen);

  lto_end_section ();
}


/* This pass is run after all of the functions are serialized and all
   of the IPA passes have written their serialized forms.  This pass
   causes the vector of all of the global decls and types used from
   this file to be written in to a section that can then be read in to
   recover these on other side.  */

static void
produce_asm_for_decls (cgraph_node_set set)
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

  /* Write out unreferenced globals, alias pairs and labels.  We defer
     doing this until now so that we can write out only what is
     needed.  */
  output_unreferenced_globals (set);

  memset (&header, 0, sizeof (struct lto_decl_header));

  section_name = lto_get_section_name (LTO_section_decls, NULL);
  lto_begin_section (section_name, !flag_wpa);
  free (section_name);

  /* Make string 0 be a NULL string.  */
  lto_output_1_stream (ob->string_stream, 0);

  /* Write the global symbols.  */
  out_state = lto_get_out_decl_state ();
  num_fns = VEC_length (lto_out_decl_state_ptr, lto_function_decl_states);
  lto_output_decl_state_streams (ob, out_state);
  for (idx = 0; idx < num_fns; idx++)
    {
      fn_out_state =
	VEC_index (lto_out_decl_state_ptr, lto_function_decl_states, idx);
      lto_output_decl_state_streams (ob, fn_out_state);
    }

  header.lto_header.major_version = LTO_major_version;
  header.lto_header.minor_version = LTO_minor_version;
  header.lto_header.section_type = LTO_section_decls;

  /* Currently not used.  This field would allow us to preallocate
     the globals vector, so that it need not be resized as it is extended.  */
  header.num_nodes = -1;

  /* Compute the total size of all decl out states. */
  decl_state_size = sizeof (int32_t);
  decl_state_size += lto_out_decl_state_written_size (out_state);
  for (idx = 0; idx < num_fns; idx++)
    {
      fn_out_state =
	VEC_index (lto_out_decl_state_ptr, lto_function_decl_states, idx);
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
  decl_state_stream = ((struct lto_output_stream *)
		       xcalloc (1, sizeof (struct lto_output_stream)));
  num_decl_states = num_fns + 1;
  lto_output_data_stream (decl_state_stream, &num_decl_states,
			  sizeof (num_decl_states));
  lto_output_decl_state_refs (ob, decl_state_stream, out_state);
  for (idx = 0; idx < num_fns; idx++)
    {
      fn_out_state =
	VEC_index (lto_out_decl_state_ptr, lto_function_decl_states, idx);
      lto_output_decl_state_refs (ob, decl_state_stream, fn_out_state);
    }
  lto_write_stream (decl_state_stream);
  free(decl_state_stream);

  lto_write_stream (ob->main_stream);
  lto_write_stream (ob->string_stream);

  lto_end_section ();

  /* Write the symbol table. */
  produce_symtab (ob->writer_cache);

  /* Write command line opts.  */
  lto_write_options ();

  /* Deallocate memory and clean up.  */
  lto_cgraph_encoder_delete (ob->decl_state->cgraph_node_encoder);
  VEC_free (lto_out_decl_state_ptr, heap, lto_function_decl_states);
  lto_function_decl_states = NULL;
  destroy_output_block (ob);
}


struct ipa_opt_pass_d pass_ipa_lto_finish_out =
{
 {
  IPA_PASS,
  "lto_decls_out",	                /* name */
  gate_lto_out,			        /* gate */
  NULL,        	                        /* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_IPA_LTO_DECL_IO,		        /* tv_id */
  0,	                                /* properties_required */
  0,					/* properties_provided */
  0,					/* properties_destroyed */
  0,            			/* todo_flags_start */
  0                                     /* todo_flags_finish */
 },
 NULL,		                        /* generate_summary */
 produce_asm_for_decls,			/* write_summary */
 NULL,		         		/* read_summary */
 NULL,					/* function_read_summary */
 NULL,					/* stmt_fixup */
 0,					/* TODOs */
 NULL,			                /* function_transform */
 NULL					/* variable_transform */
};
