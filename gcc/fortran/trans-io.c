/* IO Code translation/library interface
   Copyright (C) 2002-2018 Free Software Foundation, Inc.
   Contributed by Paul Brook

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
#include "tree.h"
#include "gfortran.h"
#include "trans.h"
#include "stringpool.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "trans-stmt.h"
#include "trans-array.h"
#include "trans-types.h"
#include "trans-const.h"
#include "options.h"

/* Members of the ioparm structure.  */

enum ioparam_type
{
  IOPARM_ptype_common,
  IOPARM_ptype_open,
  IOPARM_ptype_close,
  IOPARM_ptype_filepos,
  IOPARM_ptype_inquire,
  IOPARM_ptype_dt,
  IOPARM_ptype_wait,
  IOPARM_ptype_num
};

enum iofield_type
{
  IOPARM_type_int4,
  IOPARM_type_intio,
  IOPARM_type_pint4,
  IOPARM_type_pintio,
  IOPARM_type_pchar,
  IOPARM_type_parray,
  IOPARM_type_pad,
  IOPARM_type_char1,
  IOPARM_type_char2,
  IOPARM_type_common,
  IOPARM_type_num
};

typedef struct GTY(()) gfc_st_parameter_field {
  const char *name;
  unsigned int mask;
  enum ioparam_type param_type;
  enum iofield_type type;
  tree field;
  tree field_len;
}
gfc_st_parameter_field;

typedef struct GTY(()) gfc_st_parameter {
  const char *name;
  tree type;
}
gfc_st_parameter;

enum iofield
{
#define IOPARM(param_type, name, mask, type) IOPARM_##param_type##_##name,
#include "ioparm.def"
#undef IOPARM
  IOPARM_field_num
};

static GTY(()) gfc_st_parameter st_parameter[] =
{
  { "common", NULL },
  { "open", NULL },
  { "close", NULL },
  { "filepos", NULL },
  { "inquire", NULL },
  { "dt", NULL },
  { "wait", NULL }
};

static GTY(()) gfc_st_parameter_field st_parameter_field[] =
{
#define IOPARM(param_type, name, mask, type) \
  { #name, mask, IOPARM_ptype_##param_type, IOPARM_type_##type, NULL, NULL },
#include "ioparm.def"
#undef IOPARM
  { NULL, 0, (enum ioparam_type) 0, (enum iofield_type) 0, NULL, NULL }
};

/* Library I/O subroutines */

enum iocall
{
  IOCALL_READ,
  IOCALL_READ_DONE,
  IOCALL_WRITE,
  IOCALL_WRITE_DONE,
  IOCALL_X_INTEGER,
  IOCALL_X_INTEGER_WRITE,
  IOCALL_X_LOGICAL,
  IOCALL_X_LOGICAL_WRITE,
  IOCALL_X_CHARACTER,
  IOCALL_X_CHARACTER_WRITE,
  IOCALL_X_CHARACTER_WIDE,
  IOCALL_X_CHARACTER_WIDE_WRITE,
  IOCALL_X_REAL,
  IOCALL_X_REAL_WRITE,
  IOCALL_X_COMPLEX,
  IOCALL_X_COMPLEX_WRITE,
  IOCALL_X_REAL128,
  IOCALL_X_REAL128_WRITE,
  IOCALL_X_COMPLEX128,
  IOCALL_X_COMPLEX128_WRITE,
  IOCALL_X_ARRAY,
  IOCALL_X_ARRAY_WRITE,
  IOCALL_X_DERIVED,
  IOCALL_OPEN,
  IOCALL_CLOSE,
  IOCALL_INQUIRE,
  IOCALL_IOLENGTH,
  IOCALL_IOLENGTH_DONE,
  IOCALL_REWIND,
  IOCALL_BACKSPACE,
  IOCALL_ENDFILE,
  IOCALL_FLUSH,
  IOCALL_SET_NML_VAL,
  IOCALL_SET_NML_DTIO_VAL,
  IOCALL_SET_NML_VAL_DIM,
  IOCALL_WAIT,
  IOCALL_NUM
};

static GTY(()) tree iocall[IOCALL_NUM];

/* Variable for keeping track of what the last data transfer statement
   was.  Used for deciding which subroutine to call when the data
   transfer is complete.  */
static enum { READ, WRITE, IOLENGTH } last_dt;

/* The data transfer parameter block that should be shared by all
   data transfer calls belonging to the same read/write/iolength.  */
static GTY(()) tree dt_parm;
static stmtblock_t *dt_post_end_block;

static void
gfc_build_st_parameter (enum ioparam_type ptype, tree *types)
{
  unsigned int type;
  gfc_st_parameter_field *p;
  char name[64];
  size_t len;
  tree t = make_node (RECORD_TYPE);
  tree *chain = NULL;

  len = strlen (st_parameter[ptype].name);
  gcc_assert (len <= sizeof (name) - sizeof ("__st_parameter_"));
  memcpy (name, "__st_parameter_", sizeof ("__st_parameter_"));
  memcpy (name + sizeof ("__st_parameter_") - 1, st_parameter[ptype].name,
	  len + 1);
  TYPE_NAME (t) = get_identifier (name);

  for (type = 0, p = st_parameter_field; type < IOPARM_field_num; type++, p++)
    if (p->param_type == ptype)
      switch (p->type)
	{
	case IOPARM_type_int4:
	case IOPARM_type_intio:
	case IOPARM_type_pint4:
	case IOPARM_type_pintio:
	case IOPARM_type_parray:
	case IOPARM_type_pchar:
	case IOPARM_type_pad:
	  p->field = gfc_add_field_to_struct (t, get_identifier (p->name),
					      types[p->type], &chain);
	  break;
	case IOPARM_type_char1:
	  p->field = gfc_add_field_to_struct (t, get_identifier (p->name),
					      pchar_type_node, &chain);
	  /* FALLTHROUGH */
	case IOPARM_type_char2:
	  len = strlen (p->name);
	  gcc_assert (len <= sizeof (name) - sizeof ("_len"));
	  memcpy (name, p->name, len);
	  memcpy (name + len, "_len", sizeof ("_len"));
	  p->field_len = gfc_add_field_to_struct (t, get_identifier (name),
						  gfc_charlen_type_node,
						  &chain);
	  if (p->type == IOPARM_type_char2)
	    p->field = gfc_add_field_to_struct (t, get_identifier (p->name),
						pchar_type_node, &chain);
	  break;
	case IOPARM_type_common:
	  p->field
	    = gfc_add_field_to_struct (t,
				       get_identifier (p->name),
				       st_parameter[IOPARM_ptype_common].type,
				       &chain);
	  break;
	case IOPARM_type_num:
	  gcc_unreachable ();
	}

  /* -Wpadded warnings on these artificially created structures are not
     helpful; suppress them. */
  int save_warn_padded = warn_padded;
  warn_padded = 0;
  gfc_finish_type (t);
  warn_padded = save_warn_padded;
  st_parameter[ptype].type = t;
}


/* Build code to test an error condition and call generate_error if needed.
   Note: This builds calls to generate_error in the runtime library function.
   The function generate_error is dependent on certain parameters in the
   st_parameter_common flags to be set. (See libgfortran/runtime/error.c)
   Therefore, the code to set these flags must be generated before
   this function is used.  */

static void
gfc_trans_io_runtime_check (bool has_iostat, tree cond, tree var,
			    int error_code, const char * msgid,
			    stmtblock_t * pblock)
{
  stmtblock_t block;
  tree body;
  tree tmp;
  tree arg1, arg2, arg3;
  char *message;

  if (integer_zerop (cond))
    return;

  /* The code to generate the error.  */
  gfc_start_block (&block);

  if (has_iostat)
    gfc_add_expr_to_block (&block, build_predict_expr (PRED_FORTRAN_FAIL_IO,
						       NOT_TAKEN));
  else
    gfc_add_expr_to_block (&block, build_predict_expr (PRED_NORETURN,
						       NOT_TAKEN));

  arg1 = gfc_build_addr_expr (NULL_TREE, var);

  arg2 = build_int_cst (integer_type_node, error_code),

  message = xasprintf ("%s", _(msgid));
  arg3 = gfc_build_addr_expr (pchar_type_node,
			      gfc_build_localized_cstring_const (message));
  free (message);

  tmp = build_call_expr_loc (input_location,
			 gfor_fndecl_generate_error, 3, arg1, arg2, arg3);

  gfc_add_expr_to_block (&block, tmp);

  body = gfc_finish_block (&block);

  if (integer_onep (cond))
    {
      gfc_add_expr_to_block (pblock, body);
    }
  else
    {
      tmp = build3_v (COND_EXPR, cond, body, build_empty_stmt (input_location));
      gfc_add_expr_to_block (pblock, tmp);
    }
}


/* Create function decls for IO library functions.  */

void
gfc_build_io_library_fndecls (void)
{
  tree types[IOPARM_type_num], pad_idx, gfc_int4_type_node;
  tree gfc_intio_type_node;
  tree parm_type, dt_parm_type;
  HOST_WIDE_INT pad_size;
  unsigned int ptype;

  types[IOPARM_type_int4] = gfc_int4_type_node = gfc_get_int_type (4);
  types[IOPARM_type_intio] = gfc_intio_type_node
			    = gfc_get_int_type (gfc_intio_kind);
  types[IOPARM_type_pint4] = build_pointer_type (gfc_int4_type_node);
  types[IOPARM_type_pintio]
			    = build_pointer_type (gfc_intio_type_node);
  types[IOPARM_type_parray] = pchar_type_node;
  types[IOPARM_type_pchar] = pchar_type_node;
  pad_size = 16 * TREE_INT_CST_LOW (TYPE_SIZE_UNIT (pchar_type_node));
  pad_size += 32 * TREE_INT_CST_LOW (TYPE_SIZE_UNIT (integer_type_node));
  pad_idx = build_index_type (size_int (pad_size - 1));
  types[IOPARM_type_pad] = build_array_type (char_type_node, pad_idx);

  /* pad actually contains pointers and integers so it needs to have an
     alignment that is at least as large as the needed alignment for those
     types.  See the st_parameter_dt structure in libgfortran/io/io.h for
     what really goes into this space.  */
  SET_TYPE_ALIGN (types[IOPARM_type_pad], MAX (TYPE_ALIGN (pchar_type_node),
		     TYPE_ALIGN (gfc_get_int_type (gfc_intio_kind))));

  for (ptype = IOPARM_ptype_common; ptype < IOPARM_ptype_num; ptype++)
    gfc_build_st_parameter ((enum ioparam_type) ptype, types);

  /* Define the transfer functions.  */

  dt_parm_type = build_pointer_type (st_parameter[IOPARM_ptype_dt].type);

  iocall[IOCALL_X_INTEGER] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_integer")), ".wW",
	void_type_node, 3, dt_parm_type, pvoid_type_node, gfc_int4_type_node);

  iocall[IOCALL_X_INTEGER_WRITE] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_integer_write")), ".wR",
	void_type_node, 3, dt_parm_type, pvoid_type_node, gfc_int4_type_node);

  iocall[IOCALL_X_LOGICAL] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_logical")), ".wW",
	void_type_node, 3, dt_parm_type, pvoid_type_node, gfc_int4_type_node);

  iocall[IOCALL_X_LOGICAL_WRITE] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_logical_write")), ".wR",
	void_type_node, 3, dt_parm_type, pvoid_type_node, gfc_int4_type_node);

  iocall[IOCALL_X_CHARACTER] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_character")), ".wW",
	void_type_node, 3, dt_parm_type, pvoid_type_node, gfc_charlen_type_node);

  iocall[IOCALL_X_CHARACTER_WRITE] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_character_write")), ".wR",
	void_type_node, 3, dt_parm_type, pvoid_type_node, gfc_charlen_type_node);

  iocall[IOCALL_X_CHARACTER_WIDE] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_character_wide")), ".wW",
	void_type_node, 4, dt_parm_type, pvoid_type_node,
	gfc_charlen_type_node, gfc_int4_type_node);

  iocall[IOCALL_X_CHARACTER_WIDE_WRITE] =
    gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_character_wide_write")), ".wR",
	void_type_node, 4, dt_parm_type, pvoid_type_node,
	gfc_charlen_type_node, gfc_int4_type_node);

  iocall[IOCALL_X_REAL] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_real")), ".wW",
	void_type_node, 3, dt_parm_type, pvoid_type_node, gfc_int4_type_node);

  iocall[IOCALL_X_REAL_WRITE] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_real_write")), ".wR",
	void_type_node, 3, dt_parm_type, pvoid_type_node, gfc_int4_type_node);

  iocall[IOCALL_X_COMPLEX] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_complex")), ".wW",
	void_type_node, 3, dt_parm_type, pvoid_type_node, gfc_int4_type_node);

  iocall[IOCALL_X_COMPLEX_WRITE] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_complex_write")), ".wR",
	void_type_node, 3, dt_parm_type, pvoid_type_node, gfc_int4_type_node);

  /* Version for __float128.  */
  iocall[IOCALL_X_REAL128] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_real128")), ".wW",
	void_type_node, 3, dt_parm_type, pvoid_type_node, gfc_int4_type_node);

  iocall[IOCALL_X_REAL128_WRITE] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_real128_write")), ".wR",
	void_type_node, 3, dt_parm_type, pvoid_type_node, gfc_int4_type_node);

  iocall[IOCALL_X_COMPLEX128] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_complex128")), ".wW",
	void_type_node, 3, dt_parm_type, pvoid_type_node, gfc_int4_type_node);

  iocall[IOCALL_X_COMPLEX128_WRITE] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_complex128_write")), ".wR",
	void_type_node, 3, dt_parm_type, pvoid_type_node, gfc_int4_type_node);

  iocall[IOCALL_X_ARRAY] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_array")), ".ww",
	void_type_node, 4, dt_parm_type, pvoid_type_node,
	integer_type_node, gfc_charlen_type_node);

  iocall[IOCALL_X_ARRAY_WRITE] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_array_write")), ".wr",
	void_type_node, 4, dt_parm_type, pvoid_type_node,
	integer_type_node, gfc_charlen_type_node);

  iocall[IOCALL_X_DERIVED] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("transfer_derived")), ".wrR",
	void_type_node, 2, dt_parm_type, pvoid_type_node, pchar_type_node);

  /* Library entry points */

  iocall[IOCALL_READ] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("st_read")), ".w",
	void_type_node, 1, dt_parm_type);

  iocall[IOCALL_WRITE] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("st_write")), ".w",
	void_type_node, 1, dt_parm_type);

  parm_type = build_pointer_type (st_parameter[IOPARM_ptype_open].type);
  iocall[IOCALL_OPEN] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("st_open")), ".w",
	void_type_node, 1, parm_type);

  parm_type = build_pointer_type (st_parameter[IOPARM_ptype_close].type);
  iocall[IOCALL_CLOSE] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("st_close")), ".w",
	void_type_node, 1, parm_type);

  parm_type = build_pointer_type (st_parameter[IOPARM_ptype_inquire].type);
  iocall[IOCALL_INQUIRE] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("st_inquire")), ".w",
	void_type_node, 1, parm_type);

  iocall[IOCALL_IOLENGTH] = gfc_build_library_function_decl_with_spec(
	get_identifier (PREFIX("st_iolength")), ".w",
	void_type_node, 1, dt_parm_type);

  parm_type = build_pointer_type (st_parameter[IOPARM_ptype_wait].type);
  iocall[IOCALL_WAIT] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("st_wait_async")), ".w",
	void_type_node, 1, parm_type);

  parm_type = build_pointer_type (st_parameter[IOPARM_ptype_filepos].type);
  iocall[IOCALL_REWIND] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("st_rewind")), ".w",
	void_type_node, 1, parm_type);

  iocall[IOCALL_BACKSPACE] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("st_backspace")), ".w",
	void_type_node, 1, parm_type);

  iocall[IOCALL_ENDFILE] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("st_endfile")), ".w",
	void_type_node, 1, parm_type);

  iocall[IOCALL_FLUSH] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("st_flush")), ".w",
	void_type_node, 1, parm_type);

  /* Library helpers */

  iocall[IOCALL_READ_DONE] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("st_read_done")), ".w",
	void_type_node, 1, dt_parm_type);

  iocall[IOCALL_WRITE_DONE] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("st_write_done")), ".w",
	void_type_node, 1, dt_parm_type);

  iocall[IOCALL_IOLENGTH_DONE] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("st_iolength_done")), ".w",
	void_type_node, 1, dt_parm_type);

  iocall[IOCALL_SET_NML_VAL] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("st_set_nml_var")), ".w.R",
	void_type_node, 6, dt_parm_type, pvoid_type_node, pvoid_type_node,
	gfc_int4_type_node, gfc_charlen_type_node, get_dtype_type_node());

  iocall[IOCALL_SET_NML_DTIO_VAL] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("st_set_nml_dtio_var")), ".w.R",
	void_type_node, 8, dt_parm_type, pvoid_type_node, pvoid_type_node,
	gfc_int4_type_node, gfc_charlen_type_node, get_dtype_type_node(),
	pvoid_type_node, pvoid_type_node);

  iocall[IOCALL_SET_NML_VAL_DIM] = gfc_build_library_function_decl_with_spec (
	get_identifier (PREFIX("st_set_nml_var_dim")), ".w",
	void_type_node, 5, dt_parm_type, gfc_int4_type_node,
	gfc_array_index_type, gfc_array_index_type, gfc_array_index_type);
}


static void
set_parameter_tree (stmtblock_t *block, tree var, enum iofield type, tree value)
{
  tree tmp;
  gfc_st_parameter_field *p = &st_parameter_field[type];

  if (p->param_type == IOPARM_ptype_common)
    var = fold_build3_loc (input_location, COMPONENT_REF,
			   st_parameter[IOPARM_ptype_common].type,
			   var, TYPE_FIELDS (TREE_TYPE (var)), NULL_TREE);
  tmp = fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (p->field),
			 var, p->field, NULL_TREE);
  gfc_add_modify (block, tmp, value);
}


/* Generate code to store an integer constant into the
   st_parameter_XXX structure.  */

static unsigned int
set_parameter_const (stmtblock_t *block, tree var, enum iofield type,
		     unsigned int val)
{
  gfc_st_parameter_field *p = &st_parameter_field[type];

  set_parameter_tree (block, var, type,
		      build_int_cst (TREE_TYPE (p->field), val));
  return p->mask;
}


/* Generate code to store a non-string I/O parameter into the
   st_parameter_XXX structure.  This is a pass by value.  */

static unsigned int
set_parameter_value (stmtblock_t *block, tree var, enum iofield type,
		     gfc_expr *e)
{
  gfc_se se;
  tree tmp;
  gfc_st_parameter_field *p = &st_parameter_field[type];
  tree dest_type = TREE_TYPE (p->field);

  gfc_init_se (&se, NULL);
  gfc_conv_expr_val (&se, e);

  se.expr = convert (dest_type, se.expr);
  gfc_add_block_to_block (block, &se.pre);

  if (p->param_type == IOPARM_ptype_common)
    var = fold_build3_loc (input_location, COMPONENT_REF,
			   st_parameter[IOPARM_ptype_common].type,
			   var, TYPE_FIELDS (TREE_TYPE (var)), NULL_TREE);

  tmp = fold_build3_loc (input_location, COMPONENT_REF, dest_type, var,
			 p->field, NULL_TREE);
  gfc_add_modify (block, tmp, se.expr);
  return p->mask;
}


/* Similar to set_parameter_value except generate runtime
   error checks.  */

static unsigned int
set_parameter_value_chk (stmtblock_t *block, bool has_iostat, tree var,
		     enum iofield type, gfc_expr *e)
{
  gfc_se se;
  tree tmp;
  gfc_st_parameter_field *p = &st_parameter_field[type];
  tree dest_type = TREE_TYPE (p->field);

  gfc_init_se (&se, NULL);
  gfc_conv_expr_val (&se, e);

  /* If we're storing a UNIT number, we need to check it first.  */
  if (type == IOPARM_common_unit && e->ts.kind > 4)
    {
      tree cond, val;
      int i;

      /* Don't evaluate the UNIT number multiple times.  */
      se.expr = gfc_evaluate_now (se.expr, &se.pre);

      /* UNIT numbers should be greater than the min.  */
      i = gfc_validate_kind (BT_INTEGER, 4, false);
      val = gfc_conv_mpz_to_tree (gfc_integer_kinds[i].pedantic_min_int, 4);
      cond = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
			      se.expr,
			      fold_convert (TREE_TYPE (se.expr), val));
      gfc_trans_io_runtime_check (has_iostat, cond, var, LIBERROR_BAD_UNIT,
				  "Unit number in I/O statement too small",
				  &se.pre);

      /* UNIT numbers should be less than the max.  */
      val = gfc_conv_mpz_to_tree (gfc_integer_kinds[i].huge, 4);
      cond = fold_build2_loc (input_location, GT_EXPR, logical_type_node,
			      se.expr,
			      fold_convert (TREE_TYPE (se.expr), val));
      gfc_trans_io_runtime_check (has_iostat, cond, var, LIBERROR_BAD_UNIT,
				  "Unit number in I/O statement too large",
				  &se.pre);
    }

  se.expr = convert (dest_type, se.expr);
  gfc_add_block_to_block (block, &se.pre);

  if (p->param_type == IOPARM_ptype_common)
    var = fold_build3_loc (input_location, COMPONENT_REF,
			   st_parameter[IOPARM_ptype_common].type,
			   var, TYPE_FIELDS (TREE_TYPE (var)), NULL_TREE);

  tmp = fold_build3_loc (input_location, COMPONENT_REF, dest_type, var,
			 p->field, NULL_TREE);
  gfc_add_modify (block, tmp, se.expr);
  return p->mask;
}


/* Build code to check the unit range if KIND=8 is used.  Similar to
   set_parameter_value_chk but we do not generate error calls for
   inquire statements.  */

static unsigned int
set_parameter_value_inquire (stmtblock_t *block, tree var,
			     enum iofield type, gfc_expr *e)
{
  gfc_se se;
  gfc_st_parameter_field *p = &st_parameter_field[type];
  tree dest_type = TREE_TYPE (p->field);

  gfc_init_se (&se, NULL);
  gfc_conv_expr_val (&se, e);

  /* If we're inquiring on a UNIT number, we need to check to make
     sure it exists for larger than kind = 4.  */
  if (type == IOPARM_common_unit && e->ts.kind > 4)
    {
      stmtblock_t newblock;
      tree cond1, cond2, cond3, val, body;
      int i;

      /* Don't evaluate the UNIT number multiple times.  */
      se.expr = gfc_evaluate_now (se.expr, &se.pre);

      /* UNIT numbers should be greater than the min.  */
      i = gfc_validate_kind (BT_INTEGER, 4, false);
      val = gfc_conv_mpz_to_tree (gfc_integer_kinds[i].pedantic_min_int, 4);
      cond1 = build2_loc (input_location, LT_EXPR, logical_type_node,
			  se.expr,
			  fold_convert (TREE_TYPE (se.expr), val));
      /* UNIT numbers should be less than the max.  */
      val = gfc_conv_mpz_to_tree (gfc_integer_kinds[i].huge, 4);
      cond2 = build2_loc (input_location, GT_EXPR, logical_type_node,
			  se.expr,
			  fold_convert (TREE_TYPE (se.expr), val));
      cond3 = build2_loc (input_location, TRUTH_OR_EXPR,
			  logical_type_node, cond1, cond2);

      gfc_start_block (&newblock);

      /* The unit number GFC_INVALID_UNIT is reserved.  No units can
	 ever have this value.  It is used here to signal to the
	 runtime library that the inquire unit number is outside the
	 allowable range and so cannot exist.  It is needed when
	 -fdefault-integer-8 is used.  */
      set_parameter_const (&newblock, var, IOPARM_common_unit,
			   GFC_INVALID_UNIT);

      body = gfc_finish_block (&newblock);

      cond3 = gfc_unlikely (cond3, PRED_FORTRAN_FAIL_IO);
      var = build3_v (COND_EXPR, cond3, body, build_empty_stmt (input_location));
      gfc_add_expr_to_block (&se.pre, var);
    }

  se.expr = convert (dest_type, se.expr);
  gfc_add_block_to_block (block, &se.pre);

  return p->mask;
}


/* Generate code to store a non-string I/O parameter into the
   st_parameter_XXX structure.  This is pass by reference.  */

static unsigned int
set_parameter_ref (stmtblock_t *block, stmtblock_t *postblock,
		   tree var, enum iofield type, gfc_expr *e)
{
  gfc_se se;
  tree tmp, addr;
  gfc_st_parameter_field *p = &st_parameter_field[type];

  gcc_assert (e->ts.type == BT_INTEGER || e->ts.type == BT_LOGICAL);
  gfc_init_se (&se, NULL);
  gfc_conv_expr_lhs (&se, e);

  gfc_add_block_to_block (block, &se.pre);

  if (TYPE_MODE (TREE_TYPE (se.expr))
      == TYPE_MODE (TREE_TYPE (TREE_TYPE (p->field))))
    {
      addr = convert (TREE_TYPE (p->field), gfc_build_addr_expr (NULL_TREE, se.expr));

      /* If this is for the iostat variable initialize the
	 user variable to LIBERROR_OK which is zero.  */
      if (type == IOPARM_common_iostat)
	gfc_add_modify (block, se.expr,
			     build_int_cst (TREE_TYPE (se.expr), LIBERROR_OK));
    }
  else
    {
      /* The type used by the library has different size
	from the type of the variable supplied by the user.
	Need to use a temporary.  */
      tree tmpvar = gfc_create_var (TREE_TYPE (TREE_TYPE (p->field)),
				    st_parameter_field[type].name);

      /* If this is for the iostat variable, initialize the
	 user variable to LIBERROR_OK which is zero.  */
      if (type == IOPARM_common_iostat)
	gfc_add_modify (block, tmpvar,
			     build_int_cst (TREE_TYPE (tmpvar), LIBERROR_OK));

      addr = gfc_build_addr_expr (NULL_TREE, tmpvar);
	/* After the I/O operation, we set the variable from the temporary.  */
      tmp = convert (TREE_TYPE (se.expr), tmpvar);
      gfc_add_modify (postblock, se.expr, tmp);
     }

  set_parameter_tree (block, var, type, addr);
  return p->mask;
}

/* Given an array expr, find its address and length to get a string. If the
   array is full, the string's address is the address of array's first element
   and the length is the size of the whole array.  If it is an element, the
   string's address is the element's address and the length is the rest size of
   the array.  */

static void
gfc_convert_array_to_string (gfc_se * se, gfc_expr * e)
{
  tree size;

  if (e->rank == 0)
    {
      tree type, array, tmp;
      gfc_symbol *sym;
      int rank;

      /* If it is an element, we need its address and size of the rest.  */
      gcc_assert (e->expr_type == EXPR_VARIABLE);
      gcc_assert (e->ref->u.ar.type == AR_ELEMENT);
      sym = e->symtree->n.sym;
      rank = sym->as->rank - 1;
      gfc_conv_expr (se, e);

      array = sym->backend_decl;
      type = TREE_TYPE (array);

      if (GFC_ARRAY_TYPE_P (type))
	size = GFC_TYPE_ARRAY_SIZE (type);
      else
	{
	  gcc_assert (GFC_DESCRIPTOR_TYPE_P (type));
	  size = gfc_conv_array_stride (array, rank);
	  tmp = fold_build2_loc (input_location, MINUS_EXPR,
				 gfc_array_index_type,
				 gfc_conv_array_ubound (array, rank),
				 gfc_conv_array_lbound (array, rank));
	  tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type, tmp,
				 gfc_index_one_node);
	  size = fold_build2_loc (input_location, MULT_EXPR,
				  gfc_array_index_type, tmp, size);
	}
      gcc_assert (size);

      size = fold_build2_loc (input_location, MINUS_EXPR,
			      gfc_array_index_type, size,
			      TREE_OPERAND (se->expr, 1));
      se->expr = gfc_build_addr_expr (NULL_TREE, se->expr);
      tmp = TYPE_SIZE_UNIT (gfc_get_element_type (type));
      size = fold_build2_loc (input_location, MULT_EXPR,
			      gfc_array_index_type, size,
			      fold_convert (gfc_array_index_type, tmp));
      se->string_length = fold_convert (gfc_charlen_type_node, size);
      return;
    }

  gfc_conv_array_parameter (se, e, true, NULL, NULL, &size);
  se->string_length = fold_convert (gfc_charlen_type_node, size);
}


/* Generate code to store a string and its length into the
   st_parameter_XXX structure.  */

static unsigned int
set_string (stmtblock_t * block, stmtblock_t * postblock, tree var,
	    enum iofield type, gfc_expr * e)
{
  gfc_se se;
  tree tmp;
  tree io;
  tree len;
  gfc_st_parameter_field *p = &st_parameter_field[type];

  gfc_init_se (&se, NULL);

  if (p->param_type == IOPARM_ptype_common)
    var = fold_build3_loc (input_location, COMPONENT_REF,
			   st_parameter[IOPARM_ptype_common].type,
			   var, TYPE_FIELDS (TREE_TYPE (var)), NULL_TREE);
  io = fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (p->field),
		    var, p->field, NULL_TREE);
  len = fold_build3_loc (input_location, COMPONENT_REF,
			 TREE_TYPE (p->field_len),
			 var, p->field_len, NULL_TREE);

  /* Integer variable assigned a format label.  */
  if (e->ts.type == BT_INTEGER
      && e->rank == 0
      && e->symtree->n.sym->attr.assign == 1)
    {
      char * msg;
      tree cond;

      gfc_conv_label_variable (&se, e);
      tmp = GFC_DECL_STRING_LEN (se.expr);
      cond = fold_build2_loc (input_location, LT_EXPR, logical_type_node,
			      tmp, build_int_cst (TREE_TYPE (tmp), 0));

      msg = xasprintf ("Label assigned to variable '%s' (%%ld) is not a format "
		       "label", e->symtree->name);
      gfc_trans_runtime_check (true, false, cond, &se.pre, &e->where, msg,
			       fold_convert (long_integer_type_node, tmp));
      free (msg);

      gfc_add_modify (&se.pre, io,
		 fold_convert (TREE_TYPE (io), GFC_DECL_ASSIGN_ADDR (se.expr)));
      gfc_add_modify (&se.pre, len, GFC_DECL_STRING_LEN (se.expr));
    }
  else
    {
      /* General character.  */
      if (e->ts.type == BT_CHARACTER && e->rank == 0)
	gfc_conv_expr (&se, e);
      /* Array assigned Hollerith constant or character array.  */
      else if (e->rank > 0 || (e->symtree && e->symtree->n.sym->as->rank > 0))
	gfc_convert_array_to_string (&se, e);
      else
	gcc_unreachable ();

      gfc_conv_string_parameter (&se);
      gfc_add_modify (&se.pre, io, fold_convert (TREE_TYPE (io), se.expr));
      gfc_add_modify (&se.pre, len, fold_convert (TREE_TYPE (len),
						  se.string_length));
    }

  gfc_add_block_to_block (block, &se.pre);
  gfc_add_block_to_block (postblock, &se.post);
  return p->mask;
}


/* Generate code to store the character (array) and the character length
   for an internal unit.  */

static unsigned int
set_internal_unit (stmtblock_t * block, stmtblock_t * post_block,
		   tree var, gfc_expr * e)
{
  gfc_se se;
  tree io;
  tree len;
  tree desc;
  tree tmp;
  gfc_st_parameter_field *p;
  unsigned int mask;

  gfc_init_se (&se, NULL);

  p = &st_parameter_field[IOPARM_dt_internal_unit];
  mask = p->mask;
  io = fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (p->field),
			var, p->field, NULL_TREE);
  len = fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (p->field_len),
			 var, p->field_len,	NULL_TREE);
  p = &st_parameter_field[IOPARM_dt_internal_unit_desc];
  desc = fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (p->field),
			  var, p->field, NULL_TREE);

  gcc_assert (e->ts.type == BT_CHARACTER);

  /* Character scalars.  */
  if (e->rank == 0)
    {
      gfc_conv_expr (&se, e);
      gfc_conv_string_parameter (&se);
      tmp = se.expr;
      se.expr = build_int_cst (pchar_type_node, 0);
    }

  /* Character array.  */
  else if (e->rank > 0)
    {
      if (is_subref_array (e))
	{
	  /* Use a temporary for components of arrays of derived types
	     or substring array references.  */
	  gfc_conv_subref_array_arg (&se, e, 0,
		last_dt == READ ? INTENT_IN : INTENT_OUT, false);
	  tmp = build_fold_indirect_ref_loc (input_location,
					 se.expr);
	  se.expr = gfc_build_addr_expr (pchar_type_node, tmp);
	  tmp = gfc_conv_descriptor_data_get (tmp);
	}
      else
	{
	  /* Return the data pointer and rank from the descriptor.  */
	  gfc_conv_expr_descriptor (&se, e);
	  tmp = gfc_conv_descriptor_data_get (se.expr);
	  se.expr = gfc_build_addr_expr (pchar_type_node, se.expr);
	}
    }
  else
    gcc_unreachable ();

  /* The cast is needed for character substrings and the descriptor
     data.  */
  gfc_add_modify (&se.pre, io, fold_convert (TREE_TYPE (io), tmp));
  gfc_add_modify (&se.pre, len,
		       fold_convert (TREE_TYPE (len), se.string_length));
  gfc_add_modify (&se.pre, desc, se.expr);

  gfc_add_block_to_block (block, &se.pre);
  gfc_add_block_to_block (post_block, &se.post);
  return mask;
}

/* Add a case to a IO-result switch.  */

static void
add_case (int label_value, gfc_st_label * label, stmtblock_t * body)
{
  tree tmp, value;

  if (label == NULL)
    return;			/* No label, no case */

  value = build_int_cst (integer_type_node, label_value);

  /* Make a backend label for this case.  */
  tmp = gfc_build_label_decl (NULL_TREE);

  /* And the case itself.  */
  tmp = build_case_label (value, NULL_TREE, tmp);
  gfc_add_expr_to_block (body, tmp);

  /* Jump to the label.  */
  tmp = build1_v (GOTO_EXPR, gfc_get_label_decl (label));
  gfc_add_expr_to_block (body, tmp);
}


/* Generate a switch statement that branches to the correct I/O
   result label.  The last statement of an I/O call stores the
   result into a variable because there is often cleanup that
   must be done before the switch, so a temporary would have to
   be created anyway.  */

static void
io_result (stmtblock_t * block, tree var, gfc_st_label * err_label,
	   gfc_st_label * end_label, gfc_st_label * eor_label)
{
  stmtblock_t body;
  tree tmp, rc;
  gfc_st_parameter_field *p = &st_parameter_field[IOPARM_common_flags];

  /* If no labels are specified, ignore the result instead
     of building an empty switch.  */
  if (err_label == NULL
      && end_label == NULL
      && eor_label == NULL)
    return;

  /* Build a switch statement.  */
  gfc_start_block (&body);

  /* The label values here must be the same as the values
     in the library_return enum in the runtime library */
  add_case (1, err_label, &body);
  add_case (2, end_label, &body);
  add_case (3, eor_label, &body);

  tmp = gfc_finish_block (&body);

  var = fold_build3_loc (input_location, COMPONENT_REF,
			 st_parameter[IOPARM_ptype_common].type,
			 var, TYPE_FIELDS (TREE_TYPE (var)), NULL_TREE);
  rc = fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (p->field),
			var, p->field, NULL_TREE);
  rc = fold_build2_loc (input_location, BIT_AND_EXPR, TREE_TYPE (rc),
			rc, build_int_cst (TREE_TYPE (rc),
					   IOPARM_common_libreturn_mask));

  tmp = fold_build2_loc (input_location, SWITCH_EXPR, NULL_TREE, rc, tmp);

  gfc_add_expr_to_block (block, tmp);
}


/* Store the current file and line number to variables so that if a
   library call goes awry, we can tell the user where the problem is.  */

static void
set_error_locus (stmtblock_t * block, tree var, locus * where)
{
  gfc_file *f;
  tree str, locus_file;
  int line;
  gfc_st_parameter_field *p = &st_parameter_field[IOPARM_common_filename];

  locus_file = fold_build3_loc (input_location, COMPONENT_REF,
				st_parameter[IOPARM_ptype_common].type,
				var, TYPE_FIELDS (TREE_TYPE (var)), NULL_TREE);
  locus_file = fold_build3_loc (input_location, COMPONENT_REF,
				TREE_TYPE (p->field), locus_file,
				p->field, NULL_TREE);
  f = where->lb->file;
  str = gfc_build_cstring_const (f->filename);

  str = gfc_build_addr_expr (pchar_type_node, str);
  gfc_add_modify (block, locus_file, str);

  line = LOCATION_LINE (where->lb->location);
  set_parameter_const (block, var, IOPARM_common_line, line);
}


/* Translate an OPEN statement.  */

tree
gfc_trans_open (gfc_code * code)
{
  stmtblock_t block, post_block;
  gfc_open *p;
  tree tmp, var;
  unsigned int mask = 0;

  gfc_start_block (&block);
  gfc_init_block (&post_block);

  var = gfc_create_var (st_parameter[IOPARM_ptype_open].type, "open_parm");

  set_error_locus (&block, var, &code->loc);
  p = code->ext.open;

  if (p->iomsg)
    mask |= set_string (&block, &post_block, var, IOPARM_common_iomsg,
			p->iomsg);

  if (p->iostat)
    mask |= set_parameter_ref (&block, &post_block, var, IOPARM_common_iostat,
			       p->iostat);

  if (p->err)
    mask |= IOPARM_common_err;

  if (p->file)
    mask |= set_string (&block, &post_block, var, IOPARM_open_file, p->file);

  if (p->status)
    mask |= set_string (&block, &post_block, var, IOPARM_open_status,
			p->status);

  if (p->access)
    mask |= set_string (&block, &post_block, var, IOPARM_open_access,
			p->access);

  if (p->form)
    mask |= set_string (&block, &post_block, var, IOPARM_open_form, p->form);

  if (p->recl)
    mask |= set_parameter_value (&block, var, IOPARM_open_recl_in,
				 p->recl);

  if (p->blank)
    mask |= set_string (&block, &post_block, var, IOPARM_open_blank,
			p->blank);

  if (p->position)
    mask |= set_string (&block, &post_block, var, IOPARM_open_position,
			p->position);

  if (p->action)
    mask |= set_string (&block, &post_block, var, IOPARM_open_action,
			p->action);

  if (p->delim)
    mask |= set_string (&block, &post_block, var, IOPARM_open_delim,
			p->delim);

  if (p->pad)
    mask |= set_string (&block, &post_block, var, IOPARM_open_pad, p->pad);

  if (p->decimal)
    mask |= set_string (&block, &post_block, var, IOPARM_open_decimal,
			p->decimal);

  if (p->encoding)
    mask |= set_string (&block, &post_block, var, IOPARM_open_encoding,
			p->encoding);

  if (p->round)
    mask |= set_string (&block, &post_block, var, IOPARM_open_round, p->round);

  if (p->sign)
    mask |= set_string (&block, &post_block, var, IOPARM_open_sign, p->sign);

  if (p->asynchronous)
    mask |= set_string (&block, &post_block, var, IOPARM_open_asynchronous,
			p->asynchronous);

  if (p->convert)
    mask |= set_string (&block, &post_block, var, IOPARM_open_convert,
			p->convert);

  if (p->newunit)
    mask |= set_parameter_ref (&block, &post_block, var, IOPARM_open_newunit,
			       p->newunit);

  if (p->cc)
    mask |= set_string (&block, &post_block, var, IOPARM_open_cc, p->cc);

  if (p->share)
    mask |= set_string (&block, &post_block, var, IOPARM_open_share, p->share);

  mask |= set_parameter_const (&block, var, IOPARM_open_readonly, p->readonly);

  set_parameter_const (&block, var, IOPARM_common_flags, mask);

  if (p->unit)
    set_parameter_value_chk (&block, p->iostat, var, IOPARM_common_unit, p->unit);
  else
    set_parameter_const (&block, var, IOPARM_common_unit, 0);

  tmp = gfc_build_addr_expr (NULL_TREE, var);
  tmp = build_call_expr_loc (input_location,
			 iocall[IOCALL_OPEN], 1, tmp);
  gfc_add_expr_to_block (&block, tmp);

  gfc_add_block_to_block (&block, &post_block);

  io_result (&block, var, p->err, NULL, NULL);

  return gfc_finish_block (&block);
}


/* Translate a CLOSE statement.  */

tree
gfc_trans_close (gfc_code * code)
{
  stmtblock_t block, post_block;
  gfc_close *p;
  tree tmp, var;
  unsigned int mask = 0;

  gfc_start_block (&block);
  gfc_init_block (&post_block);

  var = gfc_create_var (st_parameter[IOPARM_ptype_close].type, "close_parm");

  set_error_locus (&block, var, &code->loc);
  p = code->ext.close;

  if (p->iomsg)
    mask |= set_string (&block, &post_block, var, IOPARM_common_iomsg,
			p->iomsg);

  if (p->iostat)
    mask |= set_parameter_ref (&block, &post_block, var, IOPARM_common_iostat,
			       p->iostat);

  if (p->err)
    mask |= IOPARM_common_err;

  if (p->status)
    mask |= set_string (&block, &post_block, var, IOPARM_close_status,
			p->status);

  set_parameter_const (&block, var, IOPARM_common_flags, mask);

  if (p->unit)
    set_parameter_value_chk (&block, p->iostat, var, IOPARM_common_unit, p->unit);
  else
    set_parameter_const (&block, var, IOPARM_common_unit, 0);

  tmp = gfc_build_addr_expr (NULL_TREE, var);
  tmp = build_call_expr_loc (input_location,
			 iocall[IOCALL_CLOSE], 1, tmp);
  gfc_add_expr_to_block (&block, tmp);

  gfc_add_block_to_block (&block, &post_block);

  io_result (&block, var, p->err, NULL, NULL);

  return gfc_finish_block (&block);
}


/* Common subroutine for building a file positioning statement.  */

static tree
build_filepos (tree function, gfc_code * code)
{
  stmtblock_t block, post_block;
  gfc_filepos *p;
  tree tmp, var;
  unsigned int mask = 0;

  p = code->ext.filepos;

  gfc_start_block (&block);
  gfc_init_block (&post_block);

  var = gfc_create_var (st_parameter[IOPARM_ptype_filepos].type,
			"filepos_parm");

  set_error_locus (&block, var, &code->loc);

  if (p->iomsg)
    mask |= set_string (&block, &post_block, var, IOPARM_common_iomsg,
			p->iomsg);

  if (p->iostat)
    mask |= set_parameter_ref (&block, &post_block, var,
			       IOPARM_common_iostat, p->iostat);

  if (p->err)
    mask |= IOPARM_common_err;

  set_parameter_const (&block, var, IOPARM_common_flags, mask);

  if (p->unit)
    set_parameter_value_chk (&block, p->iostat, var, IOPARM_common_unit,
			     p->unit);
  else
    set_parameter_const (&block, var, IOPARM_common_unit, 0);

  tmp = gfc_build_addr_expr (NULL_TREE, var);
  tmp = build_call_expr_loc (input_location,
			 function, 1, tmp);
  gfc_add_expr_to_block (&block, tmp);

  gfc_add_block_to_block (&block, &post_block);

  io_result (&block, var, p->err, NULL, NULL);

  return gfc_finish_block (&block);
}


/* Translate a BACKSPACE statement.  */

tree
gfc_trans_backspace (gfc_code * code)
{
  return build_filepos (iocall[IOCALL_BACKSPACE], code);
}


/* Translate an ENDFILE statement.  */

tree
gfc_trans_endfile (gfc_code * code)
{
  return build_filepos (iocall[IOCALL_ENDFILE], code);
}


/* Translate a REWIND statement.  */

tree
gfc_trans_rewind (gfc_code * code)
{
  return build_filepos (iocall[IOCALL_REWIND], code);
}


/* Translate a FLUSH statement.  */

tree
gfc_trans_flush (gfc_code * code)
{
  return build_filepos (iocall[IOCALL_FLUSH], code);
}


/* Translate the non-IOLENGTH form of an INQUIRE statement.  */

tree
gfc_trans_inquire (gfc_code * code)
{
  stmtblock_t block, post_block;
  gfc_inquire *p;
  tree tmp, var;
  unsigned int mask = 0, mask2 = 0;

  gfc_start_block (&block);
  gfc_init_block (&post_block);

  var = gfc_create_var (st_parameter[IOPARM_ptype_inquire].type,
			"inquire_parm");

  set_error_locus (&block, var, &code->loc);
  p = code->ext.inquire;

  if (p->iomsg)
    mask |= set_string (&block, &post_block, var, IOPARM_common_iomsg,
			p->iomsg);

  if (p->iostat)
    mask |= set_parameter_ref (&block, &post_block, var, IOPARM_common_iostat,
			       p->iostat);

  if (p->err)
    mask |= IOPARM_common_err;

  /* Sanity check.  */
  if (p->unit && p->file)
    gfc_error ("INQUIRE statement at %L cannot contain both FILE and UNIT specifiers", &code->loc);

  if (p->file)
    mask |= set_string (&block, &post_block, var, IOPARM_inquire_file,
			p->file);

  if (p->exist)
    mask |= set_parameter_ref (&block, &post_block, var, IOPARM_inquire_exist,
				 p->exist);

  if (p->opened)
    mask |= set_parameter_ref (&block, &post_block, var, IOPARM_inquire_opened,
			       p->opened);

  if (p->number)
    mask |= set_parameter_ref (&block, &post_block, var, IOPARM_inquire_number,
			       p->number);

  if (p->named)
    mask |= set_parameter_ref (&block, &post_block, var, IOPARM_inquire_named,
			       p->named);

  if (p->name)
    mask |= set_string (&block, &post_block, var, IOPARM_inquire_name,
			p->name);

  if (p->access)
    mask |= set_string (&block, &post_block, var, IOPARM_inquire_access,
			p->access);

  if (p->sequential)
    mask |= set_string (&block, &post_block, var, IOPARM_inquire_sequential,
			p->sequential);

  if (p->direct)
    mask |= set_string (&block, &post_block, var, IOPARM_inquire_direct,
			p->direct);

  if (p->form)
    mask |= set_string (&block, &post_block, var, IOPARM_inquire_form,
			p->form);

  if (p->formatted)
    mask |= set_string (&block, &post_block, var, IOPARM_inquire_formatted,
			p->formatted);

  if (p->unformatted)
    mask |= set_string (&block, &post_block, var, IOPARM_inquire_unformatted,
			p->unformatted);

  if (p->recl)
    mask |= set_parameter_ref (&block, &post_block, var,
			       IOPARM_inquire_recl_out, p->recl);

  if (p->nextrec)
    mask |= set_parameter_ref (&block, &post_block, var,
			       IOPARM_inquire_nextrec, p->nextrec);

  if (p->blank)
    mask |= set_string (&block, &post_block, var, IOPARM_inquire_blank,
			p->blank);

  if (p->delim)
    mask |= set_string (&block, &post_block, var, IOPARM_inquire_delim,
			p->delim);

  if (p->position)
    mask |= set_string (&block, &post_block, var, IOPARM_inquire_position,
			p->position);

  if (p->action)
    mask |= set_string (&block, &post_block, var, IOPARM_inquire_action,
			p->action);

  if (p->read)
    mask |= set_string (&block, &post_block, var, IOPARM_inquire_read,
			p->read);

  if (p->write)
    mask |= set_string (&block, &post_block, var, IOPARM_inquire_write,
			p->write);

  if (p->readwrite)
    mask |= set_string (&block, &post_block, var, IOPARM_inquire_readwrite,
			p->readwrite);

  if (p->pad)
    mask |= set_string (&block, &post_block, var, IOPARM_inquire_pad,
			p->pad);

  if (p->convert)
    mask |= set_string (&block, &post_block, var, IOPARM_inquire_convert,
			p->convert);

  if (p->strm_pos)
    mask |= set_parameter_ref (&block, &post_block, var,
			       IOPARM_inquire_strm_pos_out, p->strm_pos);

  /* The second series of flags.  */
  if (p->asynchronous)
    mask2 |= set_string (&block, &post_block, var, IOPARM_inquire_asynchronous,
			 p->asynchronous);

  if (p->decimal)
    mask2 |= set_string (&block, &post_block, var, IOPARM_inquire_decimal,
			 p->decimal);

  if (p->encoding)
    mask2 |= set_string (&block, &post_block, var, IOPARM_inquire_encoding,
			 p->encoding);

  if (p->round)
    mask2 |= set_string (&block, &post_block, var, IOPARM_inquire_round,
			 p->round);

  if (p->sign)
    mask2 |= set_string (&block, &post_block, var, IOPARM_inquire_sign,
			 p->sign);

  if (p->pending)
    mask2 |= set_parameter_ref (&block, &post_block, var,
				IOPARM_inquire_pending, p->pending);

  if (p->size)
    mask2 |= set_parameter_ref (&block, &post_block, var, IOPARM_inquire_size,
				p->size);

  if (p->id)
    mask2 |= set_parameter_ref (&block, &post_block,var, IOPARM_inquire_id,
				p->id);
  if (p->iqstream)
    mask2 |= set_string (&block, &post_block, var, IOPARM_inquire_iqstream,
			 p->iqstream);

  if (p->share)
    mask2 |= set_string (&block, &post_block, var, IOPARM_inquire_share,
			 p->share);

  if (p->cc)
    mask2 |= set_string (&block, &post_block, var, IOPARM_inquire_cc, p->cc);

  if (mask2)
    mask |= set_parameter_const (&block, var, IOPARM_inquire_flags2, mask2);

  set_parameter_const (&block, var, IOPARM_common_flags, mask);

  if (p->unit)
    {
      set_parameter_value (&block, var, IOPARM_common_unit, p->unit);
      set_parameter_value_inquire (&block, var, IOPARM_common_unit, p->unit);
    }
  else
    set_parameter_const (&block, var, IOPARM_common_unit, 0);

  tmp = gfc_build_addr_expr (NULL_TREE, var);
  tmp = build_call_expr_loc (input_location,
			 iocall[IOCALL_INQUIRE], 1, tmp);
  gfc_add_expr_to_block (&block, tmp);

  gfc_add_block_to_block (&block, &post_block);

  io_result (&block, var, p->err, NULL, NULL);

  return gfc_finish_block (&block);
}


tree
gfc_trans_wait (gfc_code * code)
{
  stmtblock_t block, post_block;
  gfc_wait *p;
  tree tmp, var;
  unsigned int mask = 0;

  gfc_start_block (&block);
  gfc_init_block (&post_block);

  var = gfc_create_var (st_parameter[IOPARM_ptype_wait].type,
			"wait_parm");

  set_error_locus (&block, var, &code->loc);
  p = code->ext.wait;

  /* Set parameters here.  */
  if (p->iomsg)
    mask |= set_string (&block, &post_block, var, IOPARM_common_iomsg,
			p->iomsg);

  if (p->iostat)
    mask |= set_parameter_ref (&block, &post_block, var, IOPARM_common_iostat,
			       p->iostat);

  if (p->err)
    mask |= IOPARM_common_err;

  if (p->id)
    mask |= set_parameter_ref (&block, &post_block, var, IOPARM_wait_id, p->id);

  set_parameter_const (&block, var, IOPARM_common_flags, mask);

  if (p->unit)
    set_parameter_value_chk (&block, p->iostat, var, IOPARM_common_unit, p->unit);

  tmp = gfc_build_addr_expr (NULL_TREE, var);
  tmp = build_call_expr_loc (input_location,
			 iocall[IOCALL_WAIT], 1, tmp);
  gfc_add_expr_to_block (&block, tmp);

  gfc_add_block_to_block (&block, &post_block);

  io_result (&block, var, p->err, NULL, NULL);

  return gfc_finish_block (&block);

}


/* nml_full_name builds up the fully qualified name of a
   derived type component. '+' is used to denote a type extension.  */

static char*
nml_full_name (const char* var_name, const char* cmp_name, bool parent)
{
  int full_name_length;
  char * full_name;

  full_name_length = strlen (var_name) + strlen (cmp_name) + 1;
  full_name = XCNEWVEC (char, full_name_length + 1);
  strcpy (full_name, var_name);
  full_name = strcat (full_name, parent ? "+" : "%");
  full_name = strcat (full_name, cmp_name);
  return full_name;
}


/* nml_get_addr_expr builds an address expression from the
   gfc_symbol or gfc_component backend_decl's. An offset is
   provided so that the address of an element of an array of
   derived types is returned. This is used in the runtime to
   determine that span of the derived type.  */

static tree
nml_get_addr_expr (gfc_symbol * sym, gfc_component * c,
		   tree base_addr)
{
  tree decl = NULL_TREE;
  tree tmp;

  if (sym)
    {
      sym->attr.referenced = 1;
      decl = gfc_get_symbol_decl (sym);

      /* If this is the enclosing function declaration, use
	 the fake result instead.  */
      if (decl == current_function_decl)
	decl = gfc_get_fake_result_decl (sym, 0);
      else if (decl == DECL_CONTEXT (current_function_decl))
	decl =  gfc_get_fake_result_decl (sym, 1);
    }
  else
    decl = c->backend_decl;

  gcc_assert (decl && (TREE_CODE (decl) == FIELD_DECL
		       || VAR_P (decl)
		       || TREE_CODE (decl) == PARM_DECL
		       || TREE_CODE (decl) == COMPONENT_REF));

  tmp = decl;

  /* Build indirect reference, if dummy argument.  */

  if (POINTER_TYPE_P (TREE_TYPE(tmp)))
    tmp = build_fold_indirect_ref_loc (input_location, tmp);

  /* Treat the component of a derived type, using base_addr for
     the derived type.  */

  if (TREE_CODE (decl) == FIELD_DECL)
    tmp = fold_build3_loc (input_location, COMPONENT_REF, TREE_TYPE (tmp),
			   base_addr, tmp, NULL_TREE);

  if (GFC_CLASS_TYPE_P (TREE_TYPE (tmp))
      && GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (gfc_class_data_get (tmp))))
    tmp = gfc_class_data_get (tmp);

  if (GFC_DESCRIPTOR_TYPE_P (TREE_TYPE (tmp)))
    tmp = gfc_conv_array_data (tmp);
  else
    {
      if (!POINTER_TYPE_P (TREE_TYPE (tmp)))
	tmp = gfc_build_addr_expr (NULL_TREE, tmp);

      if (TREE_CODE (TREE_TYPE (tmp)) == ARRAY_TYPE)
         tmp = gfc_build_array_ref (tmp, gfc_index_zero_node, NULL);

      if (!POINTER_TYPE_P (TREE_TYPE (tmp)))
	tmp = build_fold_indirect_ref_loc (input_location,
				   tmp);
    }

  gcc_assert (tmp && POINTER_TYPE_P (TREE_TYPE (tmp)));

  return tmp;
}


/* For an object VAR_NAME whose base address is BASE_ADDR, generate a
   call to iocall[IOCALL_SET_NML_VAL].  For derived type variable, recursively
   generate calls to iocall[IOCALL_SET_NML_VAL] for each component.  */

#define IARG(i) build_int_cst (gfc_array_index_type, i)

static void
transfer_namelist_element (stmtblock_t * block, const char * var_name,
			   gfc_symbol * sym, gfc_component * c,
			   tree base_addr)
{
  gfc_typespec * ts = NULL;
  gfc_array_spec * as = NULL;
  tree addr_expr = NULL;
  tree dt = NULL;
  tree string;
  tree tmp;
  tree dtype;
  tree dt_parm_addr;
  tree decl = NULL_TREE;
  tree gfc_int4_type_node = gfc_get_int_type (4);
  tree dtio_proc = null_pointer_node;
  tree vtable = null_pointer_node;
  int n_dim;
  int rank = 0;

  gcc_assert (sym || c);

  /* Build the namelist object name.  */

  string = gfc_build_cstring_const (var_name);
  string = gfc_build_addr_expr (pchar_type_node, string);

  /* Build ts, as and data address using symbol or component.  */

  ts = sym ? &sym->ts : &c->ts;

  if (ts->type != BT_CLASS)
    as = sym ? sym->as : c->as;
  else
    as = sym ? CLASS_DATA (sym)->as : CLASS_DATA (c)->as;

  addr_expr = nml_get_addr_expr (sym, c, base_addr);

  if (as)
    rank = as->rank;

  if (rank)
    {
      decl = sym ? sym->backend_decl : c->backend_decl;
      if (sym && sym->attr.dummy)
        decl = build_fold_indirect_ref_loc (input_location, decl);

      if (ts->type == BT_CLASS)
	decl = gfc_class_data_get (decl);
      dt =  TREE_TYPE (decl);
      dtype = gfc_get_dtype (dt);
    }
  else
    {
      dt =  gfc_typenode_for_spec (ts);
      dtype = gfc_get_dtype_rank_type (0, dt);
    }

  /* Build up the arguments for the transfer call.
     The call for the scalar part transfers:
     (address, name, type, kind or string_length, dtype)  */

  dt_parm_addr = gfc_build_addr_expr (NULL_TREE, dt_parm);

  /* Check if the derived type has a specific DTIO for the mode.
     Note that although namelist io is forbidden to have a format
     list, the specific subroutine is of the formatted kind.  */
  if (ts->type == BT_DERIVED || ts->type == BT_CLASS)
    {
      gfc_symbol *derived;
      if (ts->type==BT_CLASS)
	derived = ts->u.derived->components->ts.u.derived;
      else
	derived = ts->u.derived;

      gfc_symtree *tb_io_st = gfc_find_typebound_dtio_proc (derived,
							last_dt == WRITE, true);

      if (ts->type == BT_CLASS && tb_io_st)
	{
	  // polymorphic DTIO call  (based on the dynamic type)
	  gfc_se se;
	  gfc_symtree *st = gfc_find_symtree (sym->ns->sym_root, sym->name);
	  // build vtable expr
	  gfc_expr *expr = gfc_get_variable_expr (st);
	  gfc_add_vptr_component (expr);
	  gfc_init_se (&se, NULL);
	  se.want_pointer = 1;
	  gfc_conv_expr (&se, expr);
	  vtable = se.expr;
	  // build dtio expr
	  gfc_add_component_ref (expr,
				tb_io_st->n.tb->u.generic->specific_st->name);
	  gfc_init_se (&se, NULL);
	  se.want_pointer = 1;
	  gfc_conv_expr (&se, expr);
	  gfc_free_expr (expr);
	  dtio_proc = se.expr;
	}
      else
	{
	  // non-polymorphic DTIO call (based on the declared type)
	  gfc_symbol *dtio_sub = gfc_find_specific_dtio_proc (derived,
							last_dt == WRITE, true);
	  if (dtio_sub != NULL)
	    {
	      dtio_proc = gfc_get_symbol_decl (dtio_sub);
	      dtio_proc = gfc_build_addr_expr (NULL, dtio_proc);
	      gfc_symbol *vtab = gfc_find_derived_vtab (derived);
	      vtable = vtab->backend_decl;
	      if (vtable == NULL_TREE)
		vtable = gfc_get_symbol_decl (vtab);
	      vtable = gfc_build_addr_expr (pvoid_type_node, vtable);
	    }
	}
    }

  if (ts->type == BT_CHARACTER)
    tmp = ts->u.cl->backend_decl;
  else
    tmp = build_int_cst (gfc_charlen_type_node, 0);

  if (dtio_proc == null_pointer_node)
    tmp = build_call_expr_loc (input_location,
			   iocall[IOCALL_SET_NML_VAL], 6,
			   dt_parm_addr, addr_expr, string,
			   build_int_cst (gfc_int4_type_node, ts->kind),
			   tmp, dtype);
  else
    tmp = build_call_expr_loc (input_location,
			   iocall[IOCALL_SET_NML_DTIO_VAL], 8,
			   dt_parm_addr, addr_expr, string,
			   build_int_cst (gfc_int4_type_node, ts->kind),
			   tmp, dtype, dtio_proc, vtable);
  gfc_add_expr_to_block (block, tmp);

  /* If the object is an array, transfer rank times:
     (null pointer, name, stride, lbound, ubound)  */

  for ( n_dim = 0 ; n_dim < rank ; n_dim++ )
    {
      tmp = build_call_expr_loc (input_location,
			     iocall[IOCALL_SET_NML_VAL_DIM], 5,
			     dt_parm_addr,
			     build_int_cst (gfc_int4_type_node, n_dim),
			     gfc_conv_array_stride (decl, n_dim),
			     gfc_conv_array_lbound (decl, n_dim),
			     gfc_conv_array_ubound (decl, n_dim));
      gfc_add_expr_to_block (block, tmp);
    }

  if (gfc_bt_struct (ts->type) && ts->u.derived->components
      && dtio_proc == null_pointer_node)
    {
      gfc_component *cmp;

      /* Provide the RECORD_TYPE to build component references.  */

      tree expr = build_fold_indirect_ref_loc (input_location,
					   addr_expr);

      for (cmp = ts->u.derived->components; cmp; cmp = cmp->next)
	{
	  char *full_name = nml_full_name (var_name, cmp->name,
					   ts->u.derived->attr.extension);
	  transfer_namelist_element (block,
				     full_name,
				     NULL, cmp, expr);
	  free (full_name);
	}
    }
}

#undef IARG

/* Create a data transfer statement.  Not all of the fields are valid
   for both reading and writing, but improper use has been filtered
   out by now.  */

static tree
build_dt (tree function, gfc_code * code)
{
  stmtblock_t block, post_block, post_end_block, post_iu_block;
  gfc_dt *dt;
  tree tmp, var;
  gfc_expr *nmlname;
  gfc_namelist *nml;
  unsigned int mask = 0;

  gfc_start_block (&block);
  gfc_init_block (&post_block);
  gfc_init_block (&post_end_block);
  gfc_init_block (&post_iu_block);

  var = gfc_create_var (st_parameter[IOPARM_ptype_dt].type, "dt_parm");

  set_error_locus (&block, var, &code->loc);

  if (last_dt == IOLENGTH)
    {
      gfc_inquire *inq;

      inq = code->ext.inquire;

      /* First check that preconditions are met.  */
      gcc_assert (inq != NULL);
      gcc_assert (inq->iolength != NULL);

      /* Connect to the iolength variable.  */
      mask |= set_parameter_ref (&block, &post_end_block, var,
				 IOPARM_dt_iolength, inq->iolength);
      dt = NULL;
    }
  else
    {
      dt = code->ext.dt;
      gcc_assert (dt != NULL);
    }

  if (dt && dt->io_unit)
    {
      if (dt->io_unit->ts.type == BT_CHARACTER)
	{
	  mask |= set_internal_unit (&block, &post_iu_block,
				     var, dt->io_unit);
	  set_parameter_const (&block, var, IOPARM_common_unit,
			       dt->io_unit->ts.kind == 1 ?
			        GFC_INTERNAL_UNIT : GFC_INTERNAL_UNIT4);
	}
    }
  else
    set_parameter_const (&block, var, IOPARM_common_unit, 0);

  if (dt)
    {
      if (dt->iomsg)
	mask |= set_string (&block, &post_block, var, IOPARM_common_iomsg,
			    dt->iomsg);

      if (dt->iostat)
	mask |= set_parameter_ref (&block, &post_end_block, var,
				   IOPARM_common_iostat, dt->iostat);

      if (dt->err)
	mask |= IOPARM_common_err;

      if (dt->eor)
	mask |= IOPARM_common_eor;

      if (dt->end)
	mask |= IOPARM_common_end;

      if (dt->id)
	mask |= set_parameter_ref (&block, &post_end_block, var,
				   IOPARM_dt_id, dt->id);

      if (dt->pos)
	mask |= set_parameter_value (&block, var, IOPARM_dt_pos, dt->pos);

      if (dt->asynchronous)
	mask |= set_string (&block, &post_block, var,
			    IOPARM_dt_asynchronous, dt->asynchronous);

      if (dt->blank)
	mask |= set_string (&block, &post_block, var, IOPARM_dt_blank,
			    dt->blank);

      if (dt->decimal)
	mask |= set_string (&block, &post_block, var, IOPARM_dt_decimal,
			    dt->decimal);

      if (dt->delim)
	mask |= set_string (&block, &post_block, var, IOPARM_dt_delim,
			    dt->delim);

      if (dt->pad)
	mask |= set_string (&block, &post_block, var, IOPARM_dt_pad,
			    dt->pad);

      if (dt->round)
	mask |= set_string (&block, &post_block, var, IOPARM_dt_round,
			    dt->round);

      if (dt->sign)
	mask |= set_string (&block, &post_block, var, IOPARM_dt_sign,
			    dt->sign);

      if (dt->rec)
	mask |= set_parameter_value (&block, var, IOPARM_dt_rec, dt->rec);

      if (dt->advance)
	mask |= set_string (&block, &post_block, var, IOPARM_dt_advance,
			    dt->advance);

      if (dt->format_expr)
	mask |= set_string (&block, &post_end_block, var, IOPARM_dt_format,
			    dt->format_expr);

      if (dt->format_label)
	{
	  if (dt->format_label == &format_asterisk)
	    mask |= IOPARM_dt_list_format;
	  else
	    mask |= set_string (&block, &post_block, var, IOPARM_dt_format,
				dt->format_label->format);
	}

      if (dt->size)
	mask |= set_parameter_ref (&block, &post_end_block, var,
				   IOPARM_dt_size, dt->size);

      if (dt->udtio)
	mask |= IOPARM_dt_dtio;

      if (dt->dec_ext)
	mask |= IOPARM_dt_dec_ext;

      if (dt->namelist)
	{
	  if (dt->format_expr || dt->format_label)
	    gfc_internal_error ("build_dt: format with namelist");

          nmlname = gfc_get_character_expr (gfc_default_character_kind, NULL,
					    dt->namelist->name,
					    strlen (dt->namelist->name));

	  mask |= set_string (&block, &post_block, var, IOPARM_dt_namelist_name,
			      nmlname);

	  gfc_free_expr (nmlname);

	  if (last_dt == READ)
	    mask |= IOPARM_dt_namelist_read_mode;

	  set_parameter_const (&block, var, IOPARM_common_flags, mask);

	  dt_parm = var;

	  for (nml = dt->namelist->namelist; nml; nml = nml->next)
	    transfer_namelist_element (&block, nml->sym->name, nml->sym,
				       NULL, NULL_TREE);
	}
      else
	set_parameter_const (&block, var, IOPARM_common_flags, mask);

      if (dt->io_unit && dt->io_unit->ts.type == BT_INTEGER)
	set_parameter_value_chk (&block, dt->iostat, var,
				 IOPARM_common_unit, dt->io_unit);
    }
  else
    set_parameter_const (&block, var, IOPARM_common_flags, mask);

  tmp = gfc_build_addr_expr (NULL_TREE, var);
  tmp = build_call_expr_loc (UNKNOWN_LOCATION,
			 function, 1, tmp);
  gfc_add_expr_to_block (&block, tmp);

  gfc_add_block_to_block (&block, &post_block);

  dt_parm = var;
  dt_post_end_block = &post_end_block;

  /* Set implied do loop exit condition.  */
  if (last_dt == READ || last_dt == WRITE)
    {
      gfc_st_parameter_field *p = &st_parameter_field[IOPARM_common_flags];

      tmp = fold_build3_loc (input_location, COMPONENT_REF,
			     st_parameter[IOPARM_ptype_common].type,
			     dt_parm, TYPE_FIELDS (TREE_TYPE (dt_parm)),
			     NULL_TREE);
      tmp = fold_build3_loc (input_location, COMPONENT_REF,
			     TREE_TYPE (p->field), tmp, p->field, NULL_TREE);
      tmp = fold_build2_loc (input_location, BIT_AND_EXPR, TREE_TYPE (tmp),
			     tmp, build_int_cst (TREE_TYPE (tmp),
			     IOPARM_common_libreturn_mask));
    }
  else /* IOLENGTH */
    tmp = NULL_TREE;

  gfc_add_expr_to_block (&block, gfc_trans_code_cond (code->block->next, tmp));

  gfc_add_block_to_block (&block, &post_iu_block);

  dt_parm = NULL;
  dt_post_end_block = NULL;

  return gfc_finish_block (&block);
}


/* Translate the IOLENGTH form of an INQUIRE statement.  We treat
   this as a third sort of data transfer statement, except that
   lengths are summed instead of actually transferring any data.  */

tree
gfc_trans_iolength (gfc_code * code)
{
  last_dt = IOLENGTH;
  return build_dt (iocall[IOCALL_IOLENGTH], code);
}


/* Translate a READ statement.  */

tree
gfc_trans_read (gfc_code * code)
{
  last_dt = READ;
  return build_dt (iocall[IOCALL_READ], code);
}


/* Translate a WRITE statement */

tree
gfc_trans_write (gfc_code * code)
{
  last_dt = WRITE;
  return build_dt (iocall[IOCALL_WRITE], code);
}


/* Finish a data transfer statement.  */

tree
gfc_trans_dt_end (gfc_code * code)
{
  tree function, tmp;
  stmtblock_t block;

  gfc_init_block (&block);

  switch (last_dt)
    {
    case READ:
      function = iocall[IOCALL_READ_DONE];
      break;

    case WRITE:
      function = iocall[IOCALL_WRITE_DONE];
      break;

    case IOLENGTH:
      function = iocall[IOCALL_IOLENGTH_DONE];
      break;

    default:
      gcc_unreachable ();
    }

  tmp = gfc_build_addr_expr (NULL_TREE, dt_parm);
  tmp = build_call_expr_loc (input_location,
			 function, 1, tmp);
  gfc_add_expr_to_block (&block, tmp);
  gfc_add_block_to_block (&block, dt_post_end_block);
  gfc_init_block (dt_post_end_block);

  if (last_dt != IOLENGTH)
    {
      gcc_assert (code->ext.dt != NULL);
      io_result (&block, dt_parm, code->ext.dt->err,
		 code->ext.dt->end, code->ext.dt->eor);
    }

  return gfc_finish_block (&block);
}

static void
transfer_expr (gfc_se * se, gfc_typespec * ts, tree addr_expr,
	       gfc_code * code, tree vptr);

/* Given an array field in a derived type variable, generate the code
   for the loop that iterates over array elements, and the code that
   accesses those array elements.  Use transfer_expr to generate code
   for transferring that element.  Because elements may also be
   derived types, transfer_expr and transfer_array_component are mutually
   recursive.  */

static tree
transfer_array_component (tree expr, gfc_component * cm, locus * where)
{
  tree tmp;
  stmtblock_t body;
  stmtblock_t block;
  gfc_loopinfo loop;
  int n;
  gfc_ss *ss;
  gfc_se se;
  gfc_array_info *ss_array;

  gfc_start_block (&block);
  gfc_init_se (&se, NULL);

  /* Create and initialize Scalarization Status.  Unlike in
     gfc_trans_transfer, we can't simply use gfc_walk_expr to take
     care of this task, because we don't have a gfc_expr at hand.
     Build one manually, as in gfc_trans_subarray_assign.  */

  ss = gfc_get_array_ss (gfc_ss_terminator, NULL, cm->as->rank,
			 GFC_SS_COMPONENT);
  ss_array = &ss->info->data.array;

  if (cm->attr.pdt_array)
    ss_array->shape = NULL;
  else
    ss_array->shape = gfc_get_shape (cm->as->rank);

  ss_array->descriptor = expr;
  ss_array->data = gfc_conv_array_data (expr);
  ss_array->offset = gfc_conv_array_offset (expr);
  for (n = 0; n < cm->as->rank; n++)
    {
      ss_array->start[n] = gfc_conv_array_lbound (expr, n);
      ss_array->stride[n] = gfc_index_one_node;

      if (cm->attr.pdt_array)
	ss_array->end[n] = gfc_conv_array_ubound (expr, n);
      else
	{
	  mpz_init (ss_array->shape[n]);
	  mpz_sub (ss_array->shape[n], cm->as->upper[n]->value.integer,
		   cm->as->lower[n]->value.integer);
	  mpz_add_ui (ss_array->shape[n], ss_array->shape[n], 1);
	}
    }

  /* Once we got ss, we use scalarizer to create the loop.  */

  gfc_init_loopinfo (&loop);
  gfc_add_ss_to_loop (&loop, ss);
  gfc_conv_ss_startstride (&loop);
  gfc_conv_loop_setup (&loop, where);
  gfc_mark_ss_chain_used (ss, 1);
  gfc_start_scalarized_body (&loop, &body);

  gfc_copy_loopinfo_to_se (&se, &loop);
  se.ss = ss;

  /* gfc_conv_tmp_array_ref assumes that se.expr contains the array.  */
  se.expr = expr;
  gfc_conv_tmp_array_ref (&se);

  /* Now se.expr contains an element of the array.  Take the address and pass
     it to the IO routines.  */
  tmp = gfc_build_addr_expr (NULL_TREE, se.expr);
  transfer_expr (&se, &cm->ts, tmp, NULL, NULL_TREE);

  /* We are done now with the loop body.  Wrap up the scalarizer and
     return.  */

  gfc_add_block_to_block (&body, &se.pre);
  gfc_add_block_to_block (&body, &se.post);

  gfc_trans_scalarizing_loops (&loop, &body);

  gfc_add_block_to_block (&block, &loop.pre);
  gfc_add_block_to_block (&block, &loop.post);

  if (!cm->attr.pdt_array)
    {
      gcc_assert (ss_array->shape != NULL);
      gfc_free_shape (&ss_array->shape, cm->as->rank);
    }
  gfc_cleanup_loop (&loop);

  return gfc_finish_block (&block);
}


/* Helper function for transfer_expr that looks for the DTIO procedure
   either as a typebound binding or in a generic interface. If present,
   the address expression of the procedure is returned. It is assumed
   that the procedure interface has been checked during resolution.  */

static tree
get_dtio_proc (gfc_typespec * ts, gfc_code * code, gfc_symbol **dtio_sub)
{
  gfc_symbol *derived;
  bool formatted = false;
  gfc_dt *dt = code->ext.dt;

  /* Determine when to use the formatted DTIO procedure.  */
  if (dt && (dt->format_expr || dt->format_label))
    formatted = true;

  if (ts->type == BT_CLASS)
    derived = ts->u.derived->components->ts.u.derived;
  else
    derived = ts->u.derived;

  gfc_symtree *tb_io_st = gfc_find_typebound_dtio_proc (derived,
						  last_dt == WRITE, formatted);
  if (ts->type == BT_CLASS && tb_io_st)
    {
      // polymorphic DTIO call  (based on the dynamic type)
      gfc_se se;
      gfc_expr *expr = gfc_find_and_cut_at_last_class_ref (code->expr1);
      gfc_add_vptr_component (expr);
      gfc_add_component_ref (expr,
			     tb_io_st->n.tb->u.generic->specific_st->name);
      *dtio_sub = tb_io_st->n.tb->u.generic->specific->u.specific->n.sym;
      gfc_init_se (&se, NULL);
      se.want_pointer = 1;
      gfc_conv_expr (&se, expr);
      gfc_free_expr (expr);
      return se.expr;
    }
  else
    {
      // non-polymorphic DTIO call (based on the declared type)
      *dtio_sub = gfc_find_specific_dtio_proc (derived, last_dt == WRITE,
					      formatted);

      if (*dtio_sub)
	return gfc_build_addr_expr (NULL, gfc_get_symbol_decl (*dtio_sub));
    }

  return NULL_TREE;
}

/* Generate the call for a scalar transfer node.  */

static void
transfer_expr (gfc_se * se, gfc_typespec * ts, tree addr_expr,
	       gfc_code * code, tree vptr)
{
  tree tmp, function, arg2, arg3, field, expr;
  gfc_component *c;
  int kind;

  /* It is possible to get a C_NULL_PTR or C_NULL_FUNPTR expression here if
     the user says something like: print *, 'c_null_ptr: ', c_null_ptr
     We need to translate the expression to a constant if it's either
     C_NULL_PTR or C_NULL_FUNPTR.  We could also get a user variable of
     type C_PTR or C_FUNPTR, in which case the ts->type may no longer be
     BT_DERIVED (could have been changed by gfc_conv_expr).  */
  if ((ts->type == BT_DERIVED || ts->type == BT_INTEGER)
      && ts->u.derived != NULL
      && (ts->is_iso_c == 1 || ts->u.derived->ts.is_iso_c == 1))
    {
      ts->type = BT_INTEGER;
      ts->kind = gfc_index_integer_kind;
    }

  /* gfortran reaches here for "print *, c_loc(xxx)".  */
  if (ts->type == BT_VOID
      && code->expr1 && code->expr1->ts.type == BT_VOID
      && code->expr1->symtree
      && strcmp (code->expr1->symtree->name, "c_loc") == 0)
    {
      ts->type = BT_INTEGER;
      ts->kind = gfc_index_integer_kind;
    }

  kind = ts->kind;
  function = NULL;
  arg2 = NULL;
  arg3 = NULL;

  switch (ts->type)
    {
    case BT_INTEGER:
      arg2 = build_int_cst (integer_type_node, kind);
      if (last_dt == READ)
	function = iocall[IOCALL_X_INTEGER];
      else
	function = iocall[IOCALL_X_INTEGER_WRITE];

      break;

    case BT_REAL:
      arg2 = build_int_cst (integer_type_node, kind);
      if (last_dt == READ)
	{
	  if (gfc_real16_is_float128 && ts->kind == 16)
	    function = iocall[IOCALL_X_REAL128];
	  else
	    function = iocall[IOCALL_X_REAL];
	}
      else
	{
	  if (gfc_real16_is_float128 && ts->kind == 16)
	    function = iocall[IOCALL_X_REAL128_WRITE];
	  else
	    function = iocall[IOCALL_X_REAL_WRITE];
	}

      break;

    case BT_COMPLEX:
      arg2 = build_int_cst (integer_type_node, kind);
      if (last_dt == READ)
	{
	  if (gfc_real16_is_float128 && ts->kind == 16)
	    function = iocall[IOCALL_X_COMPLEX128];
	  else
	    function = iocall[IOCALL_X_COMPLEX];
	}
      else
	{
	  if (gfc_real16_is_float128 && ts->kind == 16)
	    function = iocall[IOCALL_X_COMPLEX128_WRITE];
	  else
	    function = iocall[IOCALL_X_COMPLEX_WRITE];
	}

      break;

    case BT_LOGICAL:
      arg2 = build_int_cst (integer_type_node, kind);
      if (last_dt == READ)
	function = iocall[IOCALL_X_LOGICAL];
      else
	function = iocall[IOCALL_X_LOGICAL_WRITE];

      break;

    case BT_CHARACTER:
      if (kind == 4)
	{
	  if (se->string_length)
	    arg2 = se->string_length;
	  else
	    {
	      tmp = build_fold_indirect_ref_loc (input_location,
					     addr_expr);
	      gcc_assert (TREE_CODE (TREE_TYPE (tmp)) == ARRAY_TYPE);
	      arg2 = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (tmp)));
	      arg2 = fold_convert (gfc_charlen_type_node, arg2);
	    }
	  arg3 = build_int_cst (integer_type_node, kind);
	  if (last_dt == READ)
	    function = iocall[IOCALL_X_CHARACTER_WIDE];
	  else
	    function = iocall[IOCALL_X_CHARACTER_WIDE_WRITE];

	  tmp = gfc_build_addr_expr (NULL_TREE, dt_parm);
	  tmp = build_call_expr_loc (input_location,
				 function, 4, tmp, addr_expr, arg2, arg3);
	  gfc_add_expr_to_block (&se->pre, tmp);
	  gfc_add_block_to_block (&se->pre, &se->post);
	  return;
	}
      /* Fall through.  */
    case BT_HOLLERITH:
      if (se->string_length)
	arg2 = se->string_length;
      else
	{
	  tmp = build_fold_indirect_ref_loc (input_location,
					 addr_expr);
	  gcc_assert (TREE_CODE (TREE_TYPE (tmp)) == ARRAY_TYPE);
	  arg2 = TYPE_MAX_VALUE (TYPE_DOMAIN (TREE_TYPE (tmp)));
	}
      if (last_dt == READ)
	function = iocall[IOCALL_X_CHARACTER];
      else
	function = iocall[IOCALL_X_CHARACTER_WRITE];

      break;

    case_bt_struct:
    case BT_CLASS:
      if (ts->u.derived->components == NULL)
	return;
      if (gfc_bt_struct (ts->type) || ts->type == BT_CLASS)
	{
	  gfc_symbol *derived;
	  gfc_symbol *dtio_sub = NULL;
	  /* Test for a specific DTIO subroutine.  */
	  if (ts->type == BT_DERIVED)
	    derived = ts->u.derived;
	  else
	    derived = ts->u.derived->components->ts.u.derived;

	  if (derived->attr.has_dtio_procs)
	    arg2 = get_dtio_proc (ts, code, &dtio_sub);

	  if ((dtio_sub != NULL) && (last_dt != IOLENGTH))
	    {
	      tree decl;
	      decl = build_fold_indirect_ref_loc (input_location,
						  se->expr);
	      /* Remember that the first dummy of the DTIO subroutines
		 is CLASS(derived) for extensible derived types, so the
		 conversion must be done here for derived type and for
		 scalarized CLASS array element io-list objects.  */
	      if ((ts->type == BT_DERIVED
		   && !(ts->u.derived->attr.sequence
			|| ts->u.derived->attr.is_bind_c))
		  || (ts->type == BT_CLASS
		      && !GFC_CLASS_TYPE_P (TREE_TYPE (decl))))
		gfc_conv_derived_to_class (se, code->expr1,
					   dtio_sub->formal->sym->ts,
					   vptr, false, false);
	      addr_expr = se->expr;
	      function = iocall[IOCALL_X_DERIVED];
	      break;
	    }
	  else if (gfc_bt_struct (ts->type))
	    {
	      /* Recurse into the elements of the derived type.  */
	      expr = gfc_evaluate_now (addr_expr, &se->pre);
	      expr = build_fold_indirect_ref_loc (input_location, expr);

	      /* Make sure that the derived type has been built.  An external
		 function, if only referenced in an io statement, requires this
		 check (see PR58771).  */
	      if (ts->u.derived->backend_decl == NULL_TREE)
		(void) gfc_typenode_for_spec (ts);

	      for (c = ts->u.derived->components; c; c = c->next)
		{
		  /* Ignore hidden string lengths.  */
		  if (c->name[0] == '_')
		    continue;

		  field = c->backend_decl;
		  gcc_assert (field && TREE_CODE (field) == FIELD_DECL);

		  tmp = fold_build3_loc (UNKNOWN_LOCATION,
					 COMPONENT_REF, TREE_TYPE (field),
					 expr, field, NULL_TREE);

		  if (c->attr.dimension)
		    {
		      tmp = transfer_array_component (tmp, c, & code->loc);
		      gfc_add_expr_to_block (&se->pre, tmp);
		    }
		  else
		    {
		      tree strlen = NULL_TREE;

		      if (!c->attr.pointer && !c->attr.pdt_string)
			tmp = gfc_build_addr_expr (NULL_TREE, tmp);

		      /* Use the hidden string length for pdt strings.  */
		      if (c->attr.pdt_string
			  && gfc_deferred_strlen (c, &strlen)
			  && strlen != NULL_TREE)
			{
			  strlen = fold_build3_loc (UNKNOWN_LOCATION,
						    COMPONENT_REF,
						    TREE_TYPE (strlen),
						    expr, strlen, NULL_TREE);
			  se->string_length = strlen;
			}

		      transfer_expr (se, &c->ts, tmp, code, NULL_TREE);

		      /* Reset so that the pdt string length does not propagate
			 through to other strings.  */
		      if (c->attr.pdt_string && strlen)
			se->string_length = NULL_TREE;
		   }
		}
	      return;
	    }
	  /* If a CLASS object gets through to here, fall through and ICE.  */
	}
      gcc_fallthrough ();
    default:
      gfc_internal_error ("Bad IO basetype (%d)", ts->type);
    }

  tmp = gfc_build_addr_expr (NULL_TREE, dt_parm);
  tmp = build_call_expr_loc (input_location,
			 function, 3, tmp, addr_expr, arg2);
  gfc_add_expr_to_block (&se->pre, tmp);
  gfc_add_block_to_block (&se->pre, &se->post);

}


/* Generate a call to pass an array descriptor to the IO library. The
   array should be of one of the intrinsic types.  */

static void
transfer_array_desc (gfc_se * se, gfc_typespec * ts, tree addr_expr)
{
  tree tmp, charlen_arg, kind_arg, io_call;

  if (ts->type == BT_CHARACTER)
    charlen_arg = se->string_length;
  else
    charlen_arg = build_int_cst (gfc_charlen_type_node, 0);

  kind_arg = build_int_cst (integer_type_node, ts->kind);

  tmp = gfc_build_addr_expr (NULL_TREE, dt_parm);
  if (last_dt == READ)
    io_call = iocall[IOCALL_X_ARRAY];
  else
    io_call = iocall[IOCALL_X_ARRAY_WRITE];

  tmp = build_call_expr_loc (UNKNOWN_LOCATION,
			 io_call, 4,
			 tmp, addr_expr, kind_arg, charlen_arg);
  gfc_add_expr_to_block (&se->pre, tmp);
  gfc_add_block_to_block (&se->pre, &se->post);
}


/* gfc_trans_transfer()-- Translate a TRANSFER code node */

tree
gfc_trans_transfer (gfc_code * code)
{
  stmtblock_t block, body;
  gfc_loopinfo loop;
  gfc_expr *expr;
  gfc_ref *ref;
  gfc_ss *ss;
  gfc_se se;
  tree tmp;
  tree vptr;
  int n;

  gfc_start_block (&block);
  gfc_init_block (&body);

  expr = code->expr1;
  ref = NULL;
  gfc_init_se (&se, NULL);

  if (expr->rank == 0)
    {
      /* Transfer a scalar value.  */
      if (expr->ts.type == BT_CLASS)
	{
	  se.want_pointer = 1;
	  gfc_conv_expr (&se, expr);
	  vptr = gfc_get_vptr_from_expr (se.expr);
	}
      else
	{
	  vptr = NULL_TREE;
	  gfc_conv_expr_reference (&se, expr);
	}
      transfer_expr (&se, &expr->ts, se.expr, code, vptr);
    }
  else
    {
      /* Transfer an array. If it is an array of an intrinsic
	 type, pass the descriptor to the library.  Otherwise
	 scalarize the transfer.  */
      if (expr->ref && !gfc_is_proc_ptr_comp (expr))
	{
	  for (ref = expr->ref; ref && ref->type != REF_ARRAY;
	    ref = ref->next);
	  gcc_assert (ref && ref->type == REF_ARRAY);
	}

      if (expr->ts.type != BT_CLASS
	 && expr->expr_type == EXPR_VARIABLE
	 && gfc_expr_attr (expr).pointer)
	goto scalarize;


      if (!(gfc_bt_struct (expr->ts.type)
	      || expr->ts.type == BT_CLASS)
	    && ref && ref->next == NULL
	    && !is_subref_array (expr))
	{
	  bool seen_vector = false;

	  if (ref && ref->u.ar.type == AR_SECTION)
	    {
	      for (n = 0; n < ref->u.ar.dimen; n++)
		if (ref->u.ar.dimen_type[n] == DIMEN_VECTOR)
		  {
		    seen_vector = true;
		    break;
		  }
	    }

	  if (seen_vector && last_dt == READ)
	    {
	      /* Create a temp, read to that and copy it back.  */
	      gfc_conv_subref_array_arg (&se, expr, 0, INTENT_OUT, false);
	      tmp =  se.expr;
	    }
	  else
	    {
	      /* Get the descriptor.  */
	      gfc_conv_expr_descriptor (&se, expr);
	      tmp = gfc_build_addr_expr (NULL_TREE, se.expr);
	    }

	  transfer_array_desc (&se, &expr->ts, tmp);
	  goto finish_block_label;
	}

scalarize:
      /* Initialize the scalarizer.  */
      ss = gfc_walk_expr (expr);
      gfc_init_loopinfo (&loop);
      gfc_add_ss_to_loop (&loop, ss);

      /* Initialize the loop.  */
      gfc_conv_ss_startstride (&loop);
      gfc_conv_loop_setup (&loop, &code->expr1->where);

      /* The main loop body.  */
      gfc_mark_ss_chain_used (ss, 1);
      gfc_start_scalarized_body (&loop, &body);

      gfc_copy_loopinfo_to_se (&se, &loop);
      se.ss = ss;

      gfc_conv_expr_reference (&se, expr);

      if (expr->ts.type == BT_CLASS)
	vptr = gfc_get_vptr_from_expr (ss->info->data.array.descriptor);
      else
	vptr = NULL_TREE;
      transfer_expr (&se, &expr->ts, se.expr, code, vptr);
    }

 finish_block_label:

  gfc_add_block_to_block (&body, &se.pre);
  gfc_add_block_to_block (&body, &se.post);

  if (se.ss == NULL)
    tmp = gfc_finish_block (&body);
  else
    {
      gcc_assert (expr->rank != 0);
      gcc_assert (se.ss == gfc_ss_terminator);
      gfc_trans_scalarizing_loops (&loop, &body);

      gfc_add_block_to_block (&loop.pre, &loop.post);
      tmp = gfc_finish_block (&loop.pre);
      gfc_cleanup_loop (&loop);
    }

  gfc_add_expr_to_block (&block, tmp);

  return gfc_finish_block (&block);
}

#include "gt-fortran-trans-io.h"
