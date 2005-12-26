/* Backend support for Fortran 95 basic types and derived types.
   Copyright (C) 2002, 2003, 2004, 2005 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>
   and Steven Bosscher <s.bosscher@student.tudelft.nl>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA.  */

/* trans-types.c -- gfortran backend types */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tm.h"
#include "target.h"
#include "ggc.h"
#include "toplev.h"
#include "gfortran.h"
#include "trans.h"
#include "trans-types.h"
#include "trans-const.h"
#include "real.h"


#if (GFC_MAX_DIMENSIONS < 10)
#define GFC_RANK_DIGITS 1
#define GFC_RANK_PRINTF_FORMAT "%01d"
#elif (GFC_MAX_DIMENSIONS < 100)
#define GFC_RANK_DIGITS 2
#define GFC_RANK_PRINTF_FORMAT "%02d"
#else
#error If you really need >99 dimensions, continue the sequence above...
#endif

static tree gfc_get_derived_type (gfc_symbol * derived);

tree gfc_array_index_type;
tree gfc_array_range_type;
tree gfc_character1_type_node;
tree pvoid_type_node;
tree ppvoid_type_node;
tree pchar_type_node;

tree gfc_charlen_type_node;

static GTY(()) tree gfc_desc_dim_type;
static GTY(()) tree gfc_max_array_element_size;
static GTY(()) tree gfc_array_descriptor_base[GFC_MAX_DIMENSIONS];

/* Arrays for all integral and real kinds.  We'll fill this in at runtime
   after the target has a chance to process command-line options.  */

#define MAX_INT_KINDS 5
gfc_integer_info gfc_integer_kinds[MAX_INT_KINDS + 1];
gfc_logical_info gfc_logical_kinds[MAX_INT_KINDS + 1];
static GTY(()) tree gfc_integer_types[MAX_INT_KINDS + 1];
static GTY(()) tree gfc_logical_types[MAX_INT_KINDS + 1];

#define MAX_REAL_KINDS 5
gfc_real_info gfc_real_kinds[MAX_REAL_KINDS + 1];
static GTY(()) tree gfc_real_types[MAX_REAL_KINDS + 1];
static GTY(()) tree gfc_complex_types[MAX_REAL_KINDS + 1];

/* The integer kind to use for array indices.  This will be set to the
   proper value based on target information from the backend.  */

int gfc_index_integer_kind;

/* The default kinds of the various types.  */

int gfc_default_integer_kind;
int gfc_max_integer_kind;
int gfc_default_real_kind;
int gfc_default_double_kind;
int gfc_default_character_kind;
int gfc_default_logical_kind;
int gfc_default_complex_kind;
int gfc_c_int_kind;

/* Query the target to determine which machine modes are available for
   computation.  Choose KIND numbers for them.  */

void
gfc_init_kinds (void)
{
  enum machine_mode mode;
  int i_index, r_index;
  bool saw_i4 = false, saw_i8 = false;
  bool saw_r4 = false, saw_r8 = false, saw_r16 = false;

  for (i_index = 0, mode = MIN_MODE_INT; mode <= MAX_MODE_INT; mode++)
    {
      int kind, bitsize;

      if (!targetm.scalar_mode_supported_p (mode))
	continue;

      /* The middle end doesn't support constants larger than 2*HWI.
	 Perhaps the target hook shouldn't have accepted these either,
	 but just to be safe...  */
      bitsize = GET_MODE_BITSIZE (mode);
      if (bitsize > 2*HOST_BITS_PER_WIDE_INT)
	continue;

      gcc_assert (i_index != MAX_INT_KINDS);

      /* Let the kind equal the bit size divided by 8.  This insulates the
	 programmer from the underlying byte size.  */
      kind = bitsize / 8;

      if (kind == 4)
	saw_i4 = true;
      if (kind == 8)
	saw_i8 = true;

      gfc_integer_kinds[i_index].kind = kind;
      gfc_integer_kinds[i_index].radix = 2;
      gfc_integer_kinds[i_index].digits = bitsize - 1;
      gfc_integer_kinds[i_index].bit_size = bitsize;

      gfc_logical_kinds[i_index].kind = kind;
      gfc_logical_kinds[i_index].bit_size = bitsize;

      i_index += 1;
    }

  /* Set the maximum integer kind.  Used with at least BOZ constants.  */
  gfc_max_integer_kind = gfc_integer_kinds[i_index - 1].kind;

  for (r_index = 0, mode = MIN_MODE_FLOAT; mode <= MAX_MODE_FLOAT; mode++)
    {
      const struct real_format *fmt = REAL_MODE_FORMAT (mode);
      int kind;

      if (fmt == NULL)
	continue;
      if (!targetm.scalar_mode_supported_p (mode))
	continue;

      /* Only let float/double/long double go through because the fortran
	 library assumes these are the only floating point types.  */

      if (mode != TYPE_MODE (float_type_node)
	  && (mode != TYPE_MODE (double_type_node))
          && (mode != TYPE_MODE (long_double_type_node)))
	continue;

      /* Let the kind equal the precision divided by 8, rounding up.  Again,
	 this insulates the programmer from the underlying byte size.

	 Also, it effectively deals with IEEE extended formats.  There, the
	 total size of the type may equal 16, but it's got 6 bytes of padding
	 and the increased size can get in the way of a real IEEE quad format
	 which may also be supported by the target.

	 We round up so as to handle IA-64 __floatreg (RFmode), which is an
	 82 bit type.  Not to be confused with __float80 (XFmode), which is
	 an 80 bit type also supported by IA-64.  So XFmode should come out
	 to be kind=10, and RFmode should come out to be kind=11.  Egads.  */

      kind = (GET_MODE_PRECISION (mode) + 7) / 8;

      if (kind == 4)
	saw_r4 = true;
      if (kind == 8)
	saw_r8 = true;
      if (kind == 16)
	saw_r16 = true;

      /* Careful we don't stumble a wierd internal mode.  */
      gcc_assert (r_index <= 0 || gfc_real_kinds[r_index-1].kind != kind);
      /* Or have too many modes for the allocated space.  */
      gcc_assert (r_index != MAX_REAL_KINDS);

      gfc_real_kinds[r_index].kind = kind;
      gfc_real_kinds[r_index].radix = fmt->b;
      gfc_real_kinds[r_index].digits = fmt->p;
      gfc_real_kinds[r_index].min_exponent = fmt->emin;
      gfc_real_kinds[r_index].max_exponent = fmt->emax;
      if (fmt->pnan < fmt->p)
	/* This is an IBM extended double format (or the MIPS variant)
	   made up of two IEEE doubles.  The value of the long double is
	   the sum of the values of the two parts.  The most significant
	   part is required to be the value of the long double rounded
	   to the nearest double.  If we use emax of 1024 then we can't
	   represent huge(x) = (1 - b**(-p)) * b**(emax-1) * b, because
	   rounding will make the most significant part overflow.  */
	gfc_real_kinds[r_index].max_exponent = fmt->emax - 1;
      gfc_real_kinds[r_index].mode_precision = GET_MODE_PRECISION (mode);
      r_index += 1;
    }

  /* Choose the default integer kind.  We choose 4 unless the user
     directs us otherwise.  */
  if (gfc_option.flag_default_integer)
    {
      if (!saw_i8)
	fatal_error ("integer kind=8 not available for -fdefault-integer-8 option");
      gfc_default_integer_kind = 8;
    }
  else if (saw_i4)
    gfc_default_integer_kind = 4;
  else
    gfc_default_integer_kind = gfc_integer_kinds[i_index - 1].kind;

  /* Choose the default real kind.  Again, we choose 4 when possible.  */
  if (gfc_option.flag_default_real)
    {
      if (!saw_r8)
	fatal_error ("real kind=8 not available for -fdefault-real-8 option");
      gfc_default_real_kind = 8;
    }
  else if (saw_r4)
    gfc_default_real_kind = 4;
  else
    gfc_default_real_kind = gfc_real_kinds[0].kind;

  /* Choose the default double kind.  If -fdefault-real and -fdefault-double 
     are specified, we use kind=8, if it's available.  If -fdefault-real is
     specified without -fdefault-double, we use kind=16, if it's available.
     Otherwise we do not change anything.  */
  if (gfc_option.flag_default_double && !gfc_option.flag_default_real)
    fatal_error ("Use of -fdefault-double-8 requires -fdefault-real-8");

  if (gfc_option.flag_default_real && gfc_option.flag_default_double && saw_r8)
    gfc_default_double_kind = 8;
  else if (gfc_option.flag_default_real && saw_r16)
    gfc_default_double_kind = 16;
  else if (saw_r4 && saw_r8)
    gfc_default_double_kind = 8;
  else
    {
      /* F95 14.6.3.1: A nonpointer scalar object of type double precision
	 real ... occupies two contiguous numeric storage units.

	 Therefore we must be supplied a kind twice as large as we chose
	 for single precision.  There are loopholes, in that double
	 precision must *occupy* two storage units, though it doesn't have
	 to *use* two storage units.  Which means that you can make this
	 kind artificially wide by padding it.  But at present there are
	 no GCC targets for which a two-word type does not exist, so we
	 just let gfc_validate_kind abort and tell us if something breaks.  */

      gfc_default_double_kind
	= gfc_validate_kind (BT_REAL, gfc_default_real_kind * 2, false);
    }

  /* The default logical kind is constrained to be the same as the
     default integer kind.  Similarly with complex and real.  */
  gfc_default_logical_kind = gfc_default_integer_kind;
  gfc_default_complex_kind = gfc_default_real_kind;

  /* Choose the smallest integer kind for our default character.  */
  gfc_default_character_kind = gfc_integer_kinds[0].kind;

  /* Choose the integer kind the same size as "void*" for our index kind.  */
  gfc_index_integer_kind = POINTER_SIZE / 8;
  /* Pick a kind the same size as the C "int" type.  */
  gfc_c_int_kind = INT_TYPE_SIZE / 8;
}

/* Make sure that a valid kind is present.  Returns an index into the
   associated kinds array, -1 if the kind is not present.  */

static int
validate_integer (int kind)
{
  int i;

  for (i = 0; gfc_integer_kinds[i].kind != 0; i++)
    if (gfc_integer_kinds[i].kind == kind)
      return i;

  return -1;
}

static int
validate_real (int kind)
{
  int i;

  for (i = 0; gfc_real_kinds[i].kind != 0; i++)
    if (gfc_real_kinds[i].kind == kind)
      return i;

  return -1;
}

static int
validate_logical (int kind)
{
  int i;

  for (i = 0; gfc_logical_kinds[i].kind; i++)
    if (gfc_logical_kinds[i].kind == kind)
      return i;

  return -1;
}

static int
validate_character (int kind)
{
  return kind == gfc_default_character_kind ? 0 : -1;
}

/* Validate a kind given a basic type.  The return value is the same
   for the child functions, with -1 indicating nonexistence of the
   type.  If MAY_FAIL is false, then -1 is never returned, and we ICE.  */

int
gfc_validate_kind (bt type, int kind, bool may_fail)
{
  int rc;

  switch (type)
    {
    case BT_REAL:		/* Fall through */
    case BT_COMPLEX:
      rc = validate_real (kind);
      break;
    case BT_INTEGER:
      rc = validate_integer (kind);
      break;
    case BT_LOGICAL:
      rc = validate_logical (kind);
      break;
    case BT_CHARACTER:
      rc = validate_character (kind);
      break;

    default:
      gfc_internal_error ("gfc_validate_kind(): Got bad type");
    }

  if (rc < 0 && !may_fail)
    gfc_internal_error ("gfc_validate_kind(): Got bad kind");

  return rc;
}


/* Four subroutines of gfc_init_types.  Create type nodes for the given kind.
   Reuse common type nodes where possible.  Recognize if the kind matches up
   with a C type.  This will be used later in determining which routines may
   be scarfed from libm.  */

static tree
gfc_build_int_type (gfc_integer_info *info)
{
  int mode_precision = info->bit_size;

  if (mode_precision == CHAR_TYPE_SIZE)
    info->c_char = 1;
  if (mode_precision == SHORT_TYPE_SIZE)
    info->c_short = 1;
  if (mode_precision == INT_TYPE_SIZE)
    info->c_int = 1;
  if (mode_precision == LONG_TYPE_SIZE)
    info->c_long = 1;
  if (mode_precision == LONG_LONG_TYPE_SIZE)
    info->c_long_long = 1;

  if (TYPE_PRECISION (intQI_type_node) == mode_precision)
    return intQI_type_node;
  if (TYPE_PRECISION (intHI_type_node) == mode_precision)
    return intHI_type_node;
  if (TYPE_PRECISION (intSI_type_node) == mode_precision)
    return intSI_type_node;
  if (TYPE_PRECISION (intDI_type_node) == mode_precision)
    return intDI_type_node;
  if (TYPE_PRECISION (intTI_type_node) == mode_precision)
    return intTI_type_node;

  return make_signed_type (mode_precision);
}

static tree
gfc_build_real_type (gfc_real_info *info)
{
  int mode_precision = info->mode_precision;
  tree new_type;

  if (mode_precision == FLOAT_TYPE_SIZE)
    info->c_float = 1;
  if (mode_precision == DOUBLE_TYPE_SIZE)
    info->c_double = 1;
  if (mode_precision == LONG_DOUBLE_TYPE_SIZE)
    info->c_long_double = 1;

  if (TYPE_PRECISION (float_type_node) == mode_precision)
    return float_type_node;
  if (TYPE_PRECISION (double_type_node) == mode_precision)
    return double_type_node;
  if (TYPE_PRECISION (long_double_type_node) == mode_precision)
    return long_double_type_node;

  new_type = make_node (REAL_TYPE);
  TYPE_PRECISION (new_type) = mode_precision;
  layout_type (new_type);
  return new_type;
}

static tree
gfc_build_complex_type (tree scalar_type)
{
  tree new_type;

  if (scalar_type == NULL)
    return NULL;
  if (scalar_type == float_type_node)
    return complex_float_type_node;
  if (scalar_type == double_type_node)
    return complex_double_type_node;
  if (scalar_type == long_double_type_node)
    return complex_long_double_type_node;

  new_type = make_node (COMPLEX_TYPE);
  TREE_TYPE (new_type) = scalar_type;
  layout_type (new_type);
  return new_type;
}

static tree
gfc_build_logical_type (gfc_logical_info *info)
{
  int bit_size = info->bit_size;
  tree new_type;

  if (bit_size == BOOL_TYPE_SIZE)
    {
      info->c_bool = 1;
      return boolean_type_node;
    }

  new_type = make_unsigned_type (bit_size);
  TREE_SET_CODE (new_type, BOOLEAN_TYPE);
  TYPE_MAX_VALUE (new_type) = build_int_cst (new_type, 1);
  TYPE_PRECISION (new_type) = 1;

  return new_type;
}

#if 0
/* Return the bit size of the C "size_t".  */

static unsigned int
c_size_t_size (void)
{
#ifdef SIZE_TYPE  
  if (strcmp (SIZE_TYPE, "unsigned int") == 0)
    return INT_TYPE_SIZE;
  if (strcmp (SIZE_TYPE, "long unsigned int") == 0)
    return LONG_TYPE_SIZE;
  if (strcmp (SIZE_TYPE, "short unsigned int") == 0)
    return SHORT_TYPE_SIZE;
  gcc_unreachable ();
#else
  return LONG_TYPE_SIZE;
#endif
}
#endif

/* Create the backend type nodes. We map them to their
   equivalent C type, at least for now.  We also give
   names to the types here, and we push them in the
   global binding level context.*/

void
gfc_init_types (void)
{
  char name_buf[16];
  int index;
  tree type;
  unsigned n;
  unsigned HOST_WIDE_INT hi;
  unsigned HOST_WIDE_INT lo;

  /* Create and name the types.  */
#define PUSH_TYPE(name, node) \
  pushdecl (build_decl (TYPE_DECL, get_identifier (name), node))

  for (index = 0; gfc_integer_kinds[index].kind != 0; ++index)
    {
      type = gfc_build_int_type (&gfc_integer_kinds[index]);
      gfc_integer_types[index] = type;
      snprintf (name_buf, sizeof(name_buf), "int%d",
		gfc_integer_kinds[index].kind);
      PUSH_TYPE (name_buf, type);
    }

  for (index = 0; gfc_logical_kinds[index].kind != 0; ++index)
    {
      type = gfc_build_logical_type (&gfc_logical_kinds[index]);
      gfc_logical_types[index] = type;
      snprintf (name_buf, sizeof(name_buf), "logical%d",
		gfc_logical_kinds[index].kind);
      PUSH_TYPE (name_buf, type);
    }

  for (index = 0; gfc_real_kinds[index].kind != 0; index++)
    {
      type = gfc_build_real_type (&gfc_real_kinds[index]);
      gfc_real_types[index] = type;
      snprintf (name_buf, sizeof(name_buf), "real%d",
		gfc_real_kinds[index].kind);
      PUSH_TYPE (name_buf, type);

      type = gfc_build_complex_type (type);
      gfc_complex_types[index] = type;
      snprintf (name_buf, sizeof(name_buf), "complex%d",
		gfc_real_kinds[index].kind);
      PUSH_TYPE (name_buf, type);
    }

  gfc_character1_type_node = build_type_variant (unsigned_char_type_node, 
						 0, 0);
  PUSH_TYPE ("char", gfc_character1_type_node);

  PUSH_TYPE ("byte", unsigned_char_type_node);
  PUSH_TYPE ("void", void_type_node);

  /* DBX debugging output gets upset if these aren't set.  */
  if (!TYPE_NAME (integer_type_node))
    PUSH_TYPE ("c_integer", integer_type_node);
  if (!TYPE_NAME (char_type_node))
    PUSH_TYPE ("c_char", char_type_node);

#undef PUSH_TYPE

  pvoid_type_node = build_pointer_type (void_type_node);
  ppvoid_type_node = build_pointer_type (pvoid_type_node);
  pchar_type_node = build_pointer_type (gfc_character1_type_node);

  gfc_array_index_type = gfc_get_int_type (gfc_index_integer_kind);
  /* We cannot use gfc_index_zero_node in definition of gfc_array_range_type,
     since this function is called before gfc_init_constants.  */
  gfc_array_range_type
	  = build_range_type (gfc_array_index_type,
			      build_int_cst (gfc_array_index_type, 0),
			      NULL_TREE);

  /* The maximum array element size that can be handled is determined
     by the number of bits available to store this field in the array
     descriptor.  */

  n = TYPE_PRECISION (gfc_array_index_type) - GFC_DTYPE_SIZE_SHIFT;
  lo = ~ (unsigned HOST_WIDE_INT) 0;
  if (n > HOST_BITS_PER_WIDE_INT)
    hi = lo >> (2*HOST_BITS_PER_WIDE_INT - n);
  else
    hi = 0, lo >>= HOST_BITS_PER_WIDE_INT - n;
  gfc_max_array_element_size
    = build_int_cst_wide (long_unsigned_type_node, lo, hi);

  size_type_node = gfc_array_index_type;

  boolean_type_node = gfc_get_logical_type (gfc_default_logical_kind);
  boolean_true_node = build_int_cst (boolean_type_node, 1);
  boolean_false_node = build_int_cst (boolean_type_node, 0);

  /* ??? Shouldn't this be based on gfc_index_integer_kind or so?  */
  gfc_charlen_type_node = gfc_get_int_type (4);
}

/* Get the type node for the given type and kind.  */

tree
gfc_get_int_type (int kind)
{
  int index = gfc_validate_kind (BT_INTEGER, kind, true);
  return index < 0 ? 0 : gfc_integer_types[index];
}

tree
gfc_get_real_type (int kind)
{
  int index = gfc_validate_kind (BT_REAL, kind, true);
  return index < 0 ? 0 : gfc_real_types[index];
}

tree
gfc_get_complex_type (int kind)
{
  int index = gfc_validate_kind (BT_COMPLEX, kind, true);
  return index < 0 ? 0 : gfc_complex_types[index];
}

tree
gfc_get_logical_type (int kind)
{
  int index = gfc_validate_kind (BT_LOGICAL, kind, true);
  return index < 0 ? 0 : gfc_logical_types[index];
}

/* Create a character type with the given kind and length.  */

tree
gfc_get_character_type_len (int kind, tree len)
{
  tree bounds, type;

  gfc_validate_kind (BT_CHARACTER, kind, false);

  bounds = build_range_type (gfc_charlen_type_node, gfc_index_one_node, len);
  type = build_array_type (gfc_character1_type_node, bounds);
  TYPE_STRING_FLAG (type) = 1;

  return type;
}


/* Get a type node for a character kind.  */

tree
gfc_get_character_type (int kind, gfc_charlen * cl)
{
  tree len;

  len = (cl == NULL) ? NULL_TREE : cl->backend_decl;

  return gfc_get_character_type_len (kind, len);
}

/* Covert a basic type.  This will be an array for character types.  */

tree
gfc_typenode_for_spec (gfc_typespec * spec)
{
  tree basetype;

  switch (spec->type)
    {
    case BT_UNKNOWN:
      gcc_unreachable ();

    case BT_INTEGER:
      basetype = gfc_get_int_type (spec->kind);
      break;

    case BT_REAL:
      basetype = gfc_get_real_type (spec->kind);
      break;

    case BT_COMPLEX:
      basetype = gfc_get_complex_type (spec->kind);
      break;

    case BT_LOGICAL:
      basetype = gfc_get_logical_type (spec->kind);
      break;

    case BT_CHARACTER:
      basetype = gfc_get_character_type (spec->kind, spec->cl);
      break;

    case BT_DERIVED:
      basetype = gfc_get_derived_type (spec->derived);
      break;

    default:
      gcc_unreachable ();
    }
  return basetype;
}

/* Build an INT_CST for constant expressions, otherwise return NULL_TREE.  */

static tree
gfc_conv_array_bound (gfc_expr * expr)
{
  /* If expr is an integer constant, return that.  */
  if (expr != NULL && expr->expr_type == EXPR_CONSTANT)
    return gfc_conv_mpz_to_tree (expr->value.integer, gfc_index_integer_kind);

  /* Otherwise return NULL.  */
  return NULL_TREE;
}

tree
gfc_get_element_type (tree type)
{
  tree element;

  if (GFC_ARRAY_TYPE_P (type))
    {
      if (TREE_CODE (type) == POINTER_TYPE)
        type = TREE_TYPE (type);
      gcc_assert (TREE_CODE (type) == ARRAY_TYPE);
      element = TREE_TYPE (type);
    }
  else
    {
      gcc_assert (GFC_DESCRIPTOR_TYPE_P (type));
      element = GFC_TYPE_ARRAY_DATAPTR_TYPE (type);

      gcc_assert (TREE_CODE (element) == POINTER_TYPE);
      element = TREE_TYPE (element);

      gcc_assert (TREE_CODE (element) == ARRAY_TYPE);
      element = TREE_TYPE (element);
    }

  return element;
}

/* Build an array. This function is called from gfc_sym_type().
   Actually returns array descriptor type.

   Format of array descriptors is as follows:

    struct gfc_array_descriptor
    {
      array *data
      index offset;
      index dtype;
      struct descriptor_dimension dimension[N_DIM];
    }

    struct descriptor_dimension
    {
      index stride;
      index lbound;
      index ubound;
    }

   Translation code should use gfc_conv_descriptor_* rather than accessing
   the descriptor directly. Any changes to the array descriptor type will
   require changes in gfc_conv_descriptor_* and gfc_build_array_initializer.

   This is represented internally as a RECORD_TYPE. The index nodes are
   gfc_array_index_type and the data node is a pointer to the data. See below
   for the handling of character types.

   The dtype member is formatted as follows:
    rank = dtype & GFC_DTYPE_RANK_MASK // 3 bits
    type = (dtype & GFC_DTYPE_TYPE_MASK) >> GFC_DTYPE_TYPE_SHIFT // 3 bits
    size = dtype >> GFC_DTYPE_SIZE_SHIFT

   I originally used nested ARRAY_TYPE nodes to represent arrays, but this
   generated poor code for assumed/deferred size arrays.  These require
   use of PLACEHOLDER_EXPR/WITH_RECORD_EXPR, which isn't part of the GENERIC
   grammar.  Also, there is no way to explicitly set the array stride, so
   all data must be packed(1).  I've tried to mark all the functions which
   would require modification with a GCC ARRAYS comment.

   The data component points to the first element in the array.
   The offset field is the position of the origin of the array
   (ie element (0, 0 ...)).  This may be outsite the bounds of the array.

   An element is accessed by
   data[offset + index0*stride0 + index1*stride1 + index2*stride2]
   This gives good performance as the computation does not involve the
   bounds of the array.  For packed arrays, this is optimized further by
   substituting the known strides.

   This system has one problem: all array bounds must be withing 2^31 elements
   of the origin (2^63 on 64-bit machines).  For example
   integer, dimension (80000:90000, 80000:90000, 2) :: array
   may not work properly on 32-bit machines because 80000*80000 > 2^31, so
   the calculation for stride02 would overflow.  This may still work, but
   I haven't checked, and it relies on the overflow doing the right thing.

   The way to fix this problem is to access elements as follows:
   data[(index0-lbound0)*stride0 + (index1-lbound1)*stride1]
   Obviously this is much slower.  I will make this a compile time option,
   something like -fsmall-array-offsets.  Mixing code compiled with and without
   this switch will work.

   (1) This can be worked around by modifying the upper bound of the previous
   dimension.  This requires extra fields in the descriptor (both real_ubound
   and fake_ubound).  In tree.def there is mention of TYPE_SEP, which
   may allow us to do this.  However I can't find mention of this anywhere
   else.  */


/* Returns true if the array sym does not require a descriptor.  */

int
gfc_is_nodesc_array (gfc_symbol * sym)
{
  gcc_assert (sym->attr.dimension);

  /* We only want local arrays.  */
  if (sym->attr.pointer || sym->attr.allocatable)
    return 0;

  if (sym->attr.dummy)
    {
      if (sym->as->type != AS_ASSUMED_SHAPE)
        return 1;
      else
        return 0;
    }

  if (sym->attr.result || sym->attr.function)
    return 0;

  gcc_assert (sym->as->type == AS_EXPLICIT);

  return 1;
}


/* Create an array descriptor type.  */

static tree
gfc_build_array_type (tree type, gfc_array_spec * as)
{
  tree lbound[GFC_MAX_DIMENSIONS];
  tree ubound[GFC_MAX_DIMENSIONS];
  int n;

  for (n = 0; n < as->rank; n++)
    {
      /* Create expressions for the known bounds of the array.  */
      if (as->type == AS_ASSUMED_SHAPE && as->lower[n] == NULL)
        lbound[n] = gfc_index_one_node;
      else
        lbound[n] = gfc_conv_array_bound (as->lower[n]);
      ubound[n] = gfc_conv_array_bound (as->upper[n]);
    }

  return gfc_get_array_type_bounds (type, as->rank, lbound, ubound, 0);
}

/* Returns the struct descriptor_dimension type.  */

static tree
gfc_get_desc_dim_type (void)
{
  tree type;
  tree decl;
  tree fieldlist;

  if (gfc_desc_dim_type)
    return gfc_desc_dim_type;

  /* Build the type node.  */
  type = make_node (RECORD_TYPE);

  TYPE_NAME (type) = get_identifier ("descriptor_dimension");
  TYPE_PACKED (type) = 1;

  /* Consists of the stride, lbound and ubound members.  */
  decl = build_decl (FIELD_DECL,
		     get_identifier ("stride"), gfc_array_index_type);
  DECL_CONTEXT (decl) = type;
  fieldlist = decl;

  decl = build_decl (FIELD_DECL,
		     get_identifier ("lbound"), gfc_array_index_type);
  DECL_CONTEXT (decl) = type;
  fieldlist = chainon (fieldlist, decl);

  decl = build_decl (FIELD_DECL,
		     get_identifier ("ubound"), gfc_array_index_type);
  DECL_CONTEXT (decl) = type;
  fieldlist = chainon (fieldlist, decl);

  /* Finish off the type.  */
  TYPE_FIELDS (type) = fieldlist;

  gfc_finish_type (type);

  gfc_desc_dim_type = type;
  return type;
}


/* Return the DTYPE for an array.  This describes the type and type parameters
   of the array.  */
/* TODO: Only call this when the value is actually used, and make all the
   unknown cases abort.  */

tree
gfc_get_dtype (tree type)
{
  tree size;
  int n;
  HOST_WIDE_INT i;
  tree tmp;
  tree dtype;
  tree etype;
  int rank;

  gcc_assert (GFC_DESCRIPTOR_TYPE_P (type) || GFC_ARRAY_TYPE_P (type));

  if (GFC_TYPE_ARRAY_DTYPE (type))
    return GFC_TYPE_ARRAY_DTYPE (type);

  rank = GFC_TYPE_ARRAY_RANK (type);
  etype = gfc_get_element_type (type);

  switch (TREE_CODE (etype))
    {
    case INTEGER_TYPE:
      n = GFC_DTYPE_INTEGER;
      break;

    case BOOLEAN_TYPE:
      n = GFC_DTYPE_LOGICAL;
      break;

    case REAL_TYPE:
      n = GFC_DTYPE_REAL;
      break;

    case COMPLEX_TYPE:
      n = GFC_DTYPE_COMPLEX;
      break;

    /* We will never have arrays of arrays.  */
    case RECORD_TYPE:
      n = GFC_DTYPE_DERIVED;
      break;

    case ARRAY_TYPE:
      n = GFC_DTYPE_CHARACTER;
      break;

    default:
      /* TODO: Don't do dtype for temporary descriptorless arrays.  */
      /* We can strange array types for temporary arrays.  */
      return gfc_index_zero_node;
    }

  gcc_assert (rank <= GFC_DTYPE_RANK_MASK);
  size = TYPE_SIZE_UNIT (etype);

  i = rank | (n << GFC_DTYPE_TYPE_SHIFT);
  if (size && INTEGER_CST_P (size))
    {
      if (tree_int_cst_lt (gfc_max_array_element_size, size))
	internal_error ("Array element size too big");

      i += TREE_INT_CST_LOW (size) << GFC_DTYPE_SIZE_SHIFT;
    }
  dtype = build_int_cst (gfc_array_index_type, i);

  if (size && !INTEGER_CST_P (size))
    {
      tmp = build_int_cst (gfc_array_index_type, GFC_DTYPE_SIZE_SHIFT);
      tmp  = fold_build2 (LSHIFT_EXPR, gfc_array_index_type, size, tmp);
      dtype = fold_build2 (PLUS_EXPR, gfc_array_index_type, tmp, dtype);
    }
  /* If we don't know the size we leave it as zero.  This should never happen
     for anything that is actually used.  */
  /* TODO: Check this is actually true, particularly when repacking
     assumed size parameters.  */

  GFC_TYPE_ARRAY_DTYPE (type) = dtype;
  return dtype;
}


/* Build an array type for use without a descriptor.  Valid values of packed
   are 0=no, 1=partial, 2=full, 3=static.  */

tree
gfc_get_nodesc_array_type (tree etype, gfc_array_spec * as, int packed)
{
  tree range;
  tree type;
  tree tmp;
  int n;
  int known_stride;
  int known_offset;
  mpz_t offset;
  mpz_t stride;
  mpz_t delta;
  gfc_expr *expr;

  mpz_init_set_ui (offset, 0);
  mpz_init_set_ui (stride, 1);
  mpz_init (delta);

  /* We don't use build_array_type because this does not include include
     lang-specific information (i.e. the bounds of the array) when checking
     for duplicates.  */
  type = make_node (ARRAY_TYPE);

  GFC_ARRAY_TYPE_P (type) = 1;
  TYPE_LANG_SPECIFIC (type) = (struct lang_type *)
    ggc_alloc_cleared (sizeof (struct lang_type));

  known_stride = (packed != 0);
  known_offset = 1;
  for (n = 0; n < as->rank; n++)
    {
      /* Fill in the stride and bound components of the type.  */
      if (known_stride)
	tmp =  gfc_conv_mpz_to_tree (stride, gfc_index_integer_kind);
      else
        tmp = NULL_TREE;
      GFC_TYPE_ARRAY_STRIDE (type, n) = tmp;

      expr = as->lower[n];
      if (expr->expr_type == EXPR_CONSTANT)
        {
          tmp = gfc_conv_mpz_to_tree (expr->value.integer,
                                  gfc_index_integer_kind);
        }
      else
        {
          known_stride = 0;
          tmp = NULL_TREE;
        }
      GFC_TYPE_ARRAY_LBOUND (type, n) = tmp;

      if (known_stride)
	{
          /* Calculate the offset.  */
          mpz_mul (delta, stride, as->lower[n]->value.integer);
          mpz_sub (offset, offset, delta);
	}
      else
	known_offset = 0;

      expr = as->upper[n];
      if (expr && expr->expr_type == EXPR_CONSTANT)
        {
	  tmp = gfc_conv_mpz_to_tree (expr->value.integer,
			          gfc_index_integer_kind);
        }
      else
        {
          tmp = NULL_TREE;
          known_stride = 0;
        }
      GFC_TYPE_ARRAY_UBOUND (type, n) = tmp;

      if (known_stride)
        {
          /* Calculate the stride.  */
          mpz_sub (delta, as->upper[n]->value.integer,
	           as->lower[n]->value.integer);
          mpz_add_ui (delta, delta, 1);
          mpz_mul (stride, stride, delta);
        }

      /* Only the first stride is known for partial packed arrays.  */
      if (packed < 2)
        known_stride = 0;
    }

  if (known_offset)
    {
      GFC_TYPE_ARRAY_OFFSET (type) =
        gfc_conv_mpz_to_tree (offset, gfc_index_integer_kind);
    }
  else
    GFC_TYPE_ARRAY_OFFSET (type) = NULL_TREE;

  if (known_stride)
    {
      GFC_TYPE_ARRAY_SIZE (type) =
        gfc_conv_mpz_to_tree (stride, gfc_index_integer_kind);
    }
  else
    GFC_TYPE_ARRAY_SIZE (type) = NULL_TREE;

  GFC_TYPE_ARRAY_RANK (type) = as->rank;
  GFC_TYPE_ARRAY_DTYPE (type) = NULL_TREE;
  range = build_range_type (gfc_array_index_type, gfc_index_zero_node,
			    NULL_TREE);
  /* TODO: use main type if it is unbounded.  */
  GFC_TYPE_ARRAY_DATAPTR_TYPE (type) =
    build_pointer_type (build_array_type (etype, range));

  if (known_stride)
    {
      mpz_sub_ui (stride, stride, 1);
      range = gfc_conv_mpz_to_tree (stride, gfc_index_integer_kind);
    }
  else
    range = NULL_TREE;

  range = build_range_type (gfc_array_index_type, gfc_index_zero_node, range);
  TYPE_DOMAIN (type) = range;

  build_pointer_type (etype);
  TREE_TYPE (type) = etype;

  layout_type (type);

  mpz_clear (offset);
  mpz_clear (stride);
  mpz_clear (delta);

  if (packed < 3 || !known_stride)
    {
      /* For dummy arrays and automatic (heap allocated) arrays we
	 want a pointer to the array.  */
      type = build_pointer_type (type);
      GFC_ARRAY_TYPE_P (type) = 1;
      TYPE_LANG_SPECIFIC (type) = TYPE_LANG_SPECIFIC (TREE_TYPE (type));
    }
  return type;
}

/* Return or create the base type for an array descriptor.  */

static tree
gfc_get_array_descriptor_base (int dimen)
{
  tree fat_type, fieldlist, decl, arraytype;
  char name[16 + GFC_RANK_DIGITS + 1];

  gcc_assert (dimen >= 1 && dimen <= GFC_MAX_DIMENSIONS);
  if (gfc_array_descriptor_base[dimen - 1])
    return gfc_array_descriptor_base[dimen - 1];

  /* Build the type node.  */
  fat_type = make_node (RECORD_TYPE);

  sprintf (name, "array_descriptor" GFC_RANK_PRINTF_FORMAT, dimen);
  TYPE_NAME (fat_type) = get_identifier (name);

  /* Add the data member as the first element of the descriptor.  */
  decl = build_decl (FIELD_DECL, get_identifier ("data"), ptr_type_node);

  DECL_CONTEXT (decl) = fat_type;
  fieldlist = decl;

  /* Add the base component.  */
  decl = build_decl (FIELD_DECL, get_identifier ("offset"),
		     gfc_array_index_type);
  DECL_CONTEXT (decl) = fat_type;
  fieldlist = chainon (fieldlist, decl);

  /* Add the dtype component.  */
  decl = build_decl (FIELD_DECL, get_identifier ("dtype"),
		     gfc_array_index_type);
  DECL_CONTEXT (decl) = fat_type;
  fieldlist = chainon (fieldlist, decl);

  /* Build the array type for the stride and bound components.  */
  arraytype =
    build_array_type (gfc_get_desc_dim_type (),
		      build_range_type (gfc_array_index_type,
					gfc_index_zero_node,
					gfc_rank_cst[dimen - 1]));

  decl = build_decl (FIELD_DECL, get_identifier ("dim"), arraytype);
  DECL_CONTEXT (decl) = fat_type;
  fieldlist = chainon (fieldlist, decl);

  /* Finish off the type.  */
  TYPE_FIELDS (fat_type) = fieldlist;

  gfc_finish_type (fat_type);

  gfc_array_descriptor_base[dimen - 1] = fat_type;
  return fat_type;
}

/* Build an array (descriptor) type with given bounds.  */

tree
gfc_get_array_type_bounds (tree etype, int dimen, tree * lbound,
			   tree * ubound, int packed)
{
  char name[8 + GFC_RANK_DIGITS + GFC_MAX_SYMBOL_LEN];
  tree fat_type, base_type, arraytype, lower, upper, stride, tmp;
  const char *typename;
  int n;

  base_type = gfc_get_array_descriptor_base (dimen);
  fat_type = build_variant_type_copy (base_type);

  tmp = TYPE_NAME (etype);
  if (tmp && TREE_CODE (tmp) == TYPE_DECL)
    tmp = DECL_NAME (tmp);
  if (tmp)
    typename = IDENTIFIER_POINTER (tmp);
  else
    typename = "unknown";
  sprintf (name, "array" GFC_RANK_PRINTF_FORMAT "_%.*s", dimen,
	   GFC_MAX_SYMBOL_LEN, typename);
  TYPE_NAME (fat_type) = get_identifier (name);

  GFC_DESCRIPTOR_TYPE_P (fat_type) = 1;
  TYPE_LANG_SPECIFIC (fat_type) = (struct lang_type *)
    ggc_alloc_cleared (sizeof (struct lang_type));

  GFC_TYPE_ARRAY_RANK (fat_type) = dimen;
  GFC_TYPE_ARRAY_DTYPE (fat_type) = NULL_TREE;

  /* Build an array descriptor record type.  */
  if (packed != 0)
    stride = gfc_index_one_node;
  else
    stride = NULL_TREE;
  for (n = 0; n < dimen; n++)
    {
      GFC_TYPE_ARRAY_STRIDE (fat_type, n) = stride;

      if (lbound)
	lower = lbound[n];
      else
	lower = NULL_TREE;

      if (lower != NULL_TREE)
	{
	  if (INTEGER_CST_P (lower))
	    GFC_TYPE_ARRAY_LBOUND (fat_type, n) = lower;
	  else
	    lower = NULL_TREE;
	}

      upper = ubound[n];
      if (upper != NULL_TREE)
	{
	  if (INTEGER_CST_P (upper))
	    GFC_TYPE_ARRAY_UBOUND (fat_type, n) = upper;
	  else
	    upper = NULL_TREE;
	}

      if (upper != NULL_TREE && lower != NULL_TREE && stride != NULL_TREE)
	{
	  tmp = fold_build2 (MINUS_EXPR, gfc_array_index_type, upper, lower);
	  tmp = fold_build2 (PLUS_EXPR, gfc_array_index_type, tmp,
			     gfc_index_one_node);
	  stride =
	    fold_build2 (MULT_EXPR, gfc_array_index_type, tmp, stride);
	  /* Check the folding worked.  */
	  gcc_assert (INTEGER_CST_P (stride));
	}
      else
	stride = NULL_TREE;
    }
  GFC_TYPE_ARRAY_SIZE (fat_type) = stride;

  /* TODO: known offsets for descriptors.  */
  GFC_TYPE_ARRAY_OFFSET (fat_type) = NULL_TREE;

  /* We define data as an unknown size array. Much better than doing
     pointer arithmetic.  */
  arraytype =
    build_array_type (etype, gfc_array_range_type);
  arraytype = build_pointer_type (arraytype);
  GFC_TYPE_ARRAY_DATAPTR_TYPE (fat_type) = arraytype;

  return fat_type;
}

/* Build a pointer type. This function is called from gfc_sym_type().  */

static tree
gfc_build_pointer_type (gfc_symbol * sym, tree type)
{
  /* Array pointer types aren't actually pointers.  */
  if (sym->attr.dimension)
    return type;
  else
    return build_pointer_type (type);
}

/* Return the type for a symbol.  Special handling is required for character
   types to get the correct level of indirection.
   For functions return the return type.
   For subroutines return void_type_node.
   Calling this multiple times for the same symbol should be avoided,
   especially for character and array types.  */

tree
gfc_sym_type (gfc_symbol * sym)
{
  tree type;
  int byref;

  if (sym->attr.flavor == FL_PROCEDURE && !sym->attr.function)
    return void_type_node;

  if (sym->backend_decl)
    {
      if (sym->attr.function)
	return TREE_TYPE (TREE_TYPE (sym->backend_decl));
      else
	return TREE_TYPE (sym->backend_decl);
    }

  type = gfc_typenode_for_spec (&sym->ts);
  if (gfc_option.flag_f2c
      && sym->attr.function
      && sym->ts.type == BT_REAL
      && sym->ts.kind == gfc_default_real_kind
      && !sym->attr.always_explicit)
    {
      /* Special case: f2c calling conventions require that (scalar) 
	 default REAL functions return the C type double instead.  */
      sym->ts.kind = gfc_default_double_kind;
      type = gfc_typenode_for_spec (&sym->ts);
      sym->ts.kind = gfc_default_real_kind;
    }

  if (sym->attr.dummy && !sym->attr.function)
    byref = 1;
  else
    byref = 0;

  if (sym->attr.dimension)
    {
      if (gfc_is_nodesc_array (sym))
        {
	  /* If this is a character argument of unknown length, just use the
	     base type.  */
	  if (sym->ts.type != BT_CHARACTER
	      || !(sym->attr.dummy || sym->attr.function)
	      || sym->ts.cl->backend_decl)
	    {
	      type = gfc_get_nodesc_array_type (type, sym->as,
						byref ? 2 : 3);
	      byref = 0;
	    }
        }
      else
	type = gfc_build_array_type (type, sym->as);
    }
  else
    {
      if (sym->attr.allocatable || sym->attr.pointer)
	type = gfc_build_pointer_type (sym, type);
    }

  /* We currently pass all parameters by reference.
     See f95_get_function_decl.  For dummy function parameters return the
     function type.  */
  if (byref)
    {
      /* We must use pointer types for potentially absent variables.  The
	 optimizers assume a reference type argument is never NULL.  */
      if (sym->attr.optional || sym->ns->proc_name->attr.entry_master)
	type = build_pointer_type (type);
      else
	type = build_reference_type (type);
    }

  return (type);
}

/* Layout and output debug info for a record type.  */

void
gfc_finish_type (tree type)
{
  tree decl;

  decl = build_decl (TYPE_DECL, NULL_TREE, type);
  TYPE_STUB_DECL (type) = decl;
  layout_type (type);
  rest_of_type_compilation (type, 1);
  rest_of_decl_compilation (decl, 1, 0);
}

/* Add a field of given NAME and TYPE to the context of a UNION_TYPE
   or RECORD_TYPE pointed to by STYPE.  The new field is chained
   to the fieldlist pointed to by FIELDLIST.

   Returns a pointer to the new field.  */

tree
gfc_add_field_to_struct (tree *fieldlist, tree context,
			 tree name, tree type)
{
  tree decl;

  decl = build_decl (FIELD_DECL, name, type);

  DECL_CONTEXT (decl) = context;
  DECL_INITIAL (decl) = 0;
  DECL_ALIGN (decl) = 0;
  DECL_USER_ALIGN (decl) = 0;
  TREE_CHAIN (decl) = NULL_TREE;
  *fieldlist = chainon (*fieldlist, decl);

  return decl;
}


/* Copy the backend_decl and component backend_decls if
   the two derived type symbols are "equal", as described
   in 4.4.2 and resolved by gfc_compare_derived_types.  */

static int
copy_dt_decls_ifequal (gfc_symbol *from, gfc_symbol *to)
{
  gfc_component *to_cm;
  gfc_component *from_cm;

  if (from->backend_decl == NULL
	|| !gfc_compare_derived_types (from, to))
    return 0;

  to->backend_decl = from->backend_decl;

  to_cm = to->components;
  from_cm = from->components;

  for (; to_cm; to_cm = to_cm->next, from_cm = from_cm->next)
    to_cm->backend_decl = from_cm->backend_decl;

  return 1;
}


/* Build a tree node for a derived type.  If there are equal
   derived types, with different local names, these are built
   at the same time.  If an equal derived type has been built
   in a parent namespace, this is used.  */

static tree
gfc_get_derived_type (gfc_symbol * derived)
{
  tree typenode, field, field_type, fieldlist;
  gfc_component *c;
  gfc_dt_list *dt;
  gfc_namespace * ns;

  gcc_assert (derived && derived->attr.flavor == FL_DERIVED);

  /* derived->backend_decl != 0 means we saw it before, but its
     components' backend_decl may have not been built.  */
  if (derived->backend_decl)
    {
      /* Its components' backend_decl have been built.  */
      if (TYPE_FIELDS (derived->backend_decl))
        return derived->backend_decl;
      else
        typenode = derived->backend_decl;
    }
  else
    {
      /* In a module, if an equal derived type is already available in the
	 specification block, use its backend declaration and those of its
	 components, rather than building anew so that potential dummy and
	 actual arguments use the same TREE_TYPE.  Non-module structures,
	 need to be built, if found, because the order of visits to the 
	 namespaces is different.  */

      for (ns = derived->ns->parent; ns; ns = ns->parent)
	{
	  for (dt = ns->derived_types; dt; dt = dt->next)
	    {
	      if (derived->module == NULL
		    && dt->derived->backend_decl == NULL
		    && gfc_compare_derived_types (dt->derived, derived))
		gfc_get_derived_type (dt->derived);

	      if (copy_dt_decls_ifequal (dt->derived, derived))
		break;
	    }
	  if (derived->backend_decl)
	    goto other_equal_dts;
	}

      /* We see this derived type first time, so build the type node.  */
      typenode = make_node (RECORD_TYPE);
      TYPE_NAME (typenode) = get_identifier (derived->name);
      TYPE_PACKED (typenode) = gfc_option.flag_pack_derived;
      derived->backend_decl = typenode;
    }

  /* Go through the derived type components, building them as
     necessary. The reason for doing this now is that it is
     possible to recurse back to this derived type through a
     pointer component (PR24092). If this happens, the fields
     will be built and so we can return the type.  */
  for (c = derived->components; c; c = c->next)
    {
      if (c->ts.type != BT_DERIVED)
	continue;

      if (!c->pointer || c->ts.derived->backend_decl == NULL)
	c->ts.derived->backend_decl = gfc_get_derived_type (c->ts.derived);
    }

  if (TYPE_FIELDS (derived->backend_decl))
    return derived->backend_decl;

  /* Build the type member list. Install the newly created RECORD_TYPE
     node as DECL_CONTEXT of each FIELD_DECL.  */
  fieldlist = NULL_TREE;
  for (c = derived->components; c; c = c->next)
    {
      if (c->ts.type == BT_DERIVED)
        field_type = c->ts.derived->backend_decl;
      else
	{
	  if (c->ts.type == BT_CHARACTER)
	    {
	      /* Evaluate the string length.  */
	      gfc_conv_const_charlen (c->ts.cl);
	      gcc_assert (c->ts.cl->backend_decl);
	    }

	  field_type = gfc_typenode_for_spec (&c->ts);
	}

      /* This returns an array descriptor type.  Initialization may be
         required.  */
      if (c->dimension)
	{
	  if (c->pointer)
	    {
	      /* Pointers to arrays aren't actually pointer types.  The
	         descriptors are separate, but the data is common.  */
	      field_type = gfc_build_array_type (field_type, c->as);
	    }
	  else
	    field_type = gfc_get_nodesc_array_type (field_type, c->as, 3);
	}
      else if (c->pointer)
	field_type = build_pointer_type (field_type);

      field = gfc_add_field_to_struct (&fieldlist, typenode,
				       get_identifier (c->name),
				       field_type);

      DECL_PACKED (field) |= TYPE_PACKED (typenode);

      gcc_assert (field);
      if (!c->backend_decl)
	c->backend_decl = field;
    }

  /* Now we have the final fieldlist.  Record it, then lay out the
     derived type, including the fields.  */
  TYPE_FIELDS (typenode) = fieldlist;

  gfc_finish_type (typenode);

  derived->backend_decl = typenode;

other_equal_dts:
  /* Add this backend_decl to all the other, equal derived types and
     their components in this namespace.  */
  for (dt = derived->ns->derived_types; dt; dt = dt->next)
    copy_dt_decls_ifequal (derived, dt->derived);

  return derived->backend_decl;
}


int
gfc_return_by_reference (gfc_symbol * sym)
{
  if (!sym->attr.function)
    return 0;

  if (sym->attr.dimension)
    return 1;

  if (sym->ts.type == BT_CHARACTER)
    return 1;

  /* Possibly return complex numbers by reference for g77 compatibility.
     We don't do this for calls to intrinsics (as the library uses the
     -fno-f2c calling convention), nor for calls to functions which always
     require an explicit interface, as no compatibility problems can
     arise there.  */
  if (gfc_option.flag_f2c
      && sym->ts.type == BT_COMPLEX
      && !sym->attr.intrinsic && !sym->attr.always_explicit)
    return 1;
  
  return 0;
}

static tree
gfc_get_mixed_entry_union (gfc_namespace *ns)
{
  tree type;
  tree decl;
  tree fieldlist;
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_entry_list *el, *el2;

  gcc_assert (ns->proc_name->attr.mixed_entry_master);
  gcc_assert (memcmp (ns->proc_name->name, "master.", 7) == 0);

  snprintf (name, GFC_MAX_SYMBOL_LEN, "munion.%s", ns->proc_name->name + 7);

  /* Build the type node.  */
  type = make_node (UNION_TYPE);

  TYPE_NAME (type) = get_identifier (name);
  fieldlist = NULL;

  for (el = ns->entries; el; el = el->next)
    {
      /* Search for duplicates.  */
      for (el2 = ns->entries; el2 != el; el2 = el2->next)
	if (el2->sym->result == el->sym->result)
	  break;

      if (el == el2)
	{
	  decl = build_decl (FIELD_DECL,
			     get_identifier (el->sym->result->name),
			     gfc_sym_type (el->sym->result));
	  DECL_CONTEXT (decl) = type;
	  fieldlist = chainon (fieldlist, decl);
	}
    }

  /* Finish off the type.  */
  TYPE_FIELDS (type) = fieldlist;

  gfc_finish_type (type);
  return type;
}

tree
gfc_get_function_type (gfc_symbol * sym)
{
  tree type;
  tree typelist;
  gfc_formal_arglist *f;
  gfc_symbol *arg;
  int nstr;
  int alternate_return;

  /* Make sure this symbol is a function or a subroutine.  */
  gcc_assert (sym->attr.flavor == FL_PROCEDURE);

  if (sym->backend_decl)
    return TREE_TYPE (sym->backend_decl);

  nstr = 0;
  alternate_return = 0;
  typelist = NULL_TREE;

  if (sym->attr.entry_master)
    {
      /* Additional parameter for selecting an entry point.  */
      typelist = gfc_chainon_list (typelist, gfc_array_index_type);
    }

  /* Some functions we use an extra parameter for the return value.  */
  if (gfc_return_by_reference (sym))
    {
      if (sym->result)
	arg = sym->result;
      else
	arg = sym;

      if (arg->ts.type == BT_CHARACTER)
	gfc_conv_const_charlen (arg->ts.cl);

      type = gfc_sym_type (arg);
      if (arg->ts.type == BT_COMPLEX
	  || arg->attr.dimension
	  || arg->ts.type == BT_CHARACTER)
	type = build_reference_type (type);

      typelist = gfc_chainon_list (typelist, type);
      if (arg->ts.type == BT_CHARACTER)
	typelist = gfc_chainon_list (typelist, gfc_charlen_type_node);
    }

  /* Build the argument types for the function.  */
  for (f = sym->formal; f; f = f->next)
    {
      arg = f->sym;
      if (arg)
	{
	  /* Evaluate constant character lengths here so that they can be
	     included in the type.  */
	  if (arg->ts.type == BT_CHARACTER)
	    gfc_conv_const_charlen (arg->ts.cl);

	  if (arg->attr.flavor == FL_PROCEDURE)
	    {
	      type = gfc_get_function_type (arg);
	      type = build_pointer_type (type);
	    }
	  else
	    type = gfc_sym_type (arg);

	  /* Parameter Passing Convention

	     We currently pass all parameters by reference.
	     Parameters with INTENT(IN) could be passed by value.
	     The problem arises if a function is called via an implicit
	     prototype. In this situation the INTENT is not known.
	     For this reason all parameters to global functions must be
	     passed by reference.  Passing by value would potentially
	     generate bad code.  Worse there would be no way of telling that
	     this code was bad, except that it would give incorrect results.

	     Contained procedures could pass by value as these are never
	     used without an explicit interface, and connot be passed as
	     actual parameters for a dummy procedure.  */
	  if (arg->ts.type == BT_CHARACTER)
            nstr++;
	  typelist = gfc_chainon_list (typelist, type);
	}
      else
        {
          if (sym->attr.subroutine)
            alternate_return = 1;
        }
    }

  /* Add hidden string length parameters.  */
  while (nstr--)
    typelist = gfc_chainon_list (typelist, gfc_charlen_type_node);

  typelist = gfc_chainon_list (typelist, void_type_node);

  if (alternate_return)
    type = integer_type_node;
  else if (!sym->attr.function || gfc_return_by_reference (sym))
    type = void_type_node;
  else if (sym->attr.mixed_entry_master)
    type = gfc_get_mixed_entry_union (sym->ns);
  else
    type = gfc_sym_type (sym);

  type = build_function_type (type, typelist);

  return type;
}

/* Language hooks for middle-end access to type nodes.  */

/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

tree
gfc_type_for_size (unsigned bits, int unsignedp)
{
  if (!unsignedp)
    {
      int i;
      for (i = 0; i <= MAX_INT_KINDS; ++i)
	{
	  tree type = gfc_integer_types[i];
	  if (type && bits == TYPE_PRECISION (type))
	    return type;
	}
    }
  else
    {
      if (bits == TYPE_PRECISION (unsigned_intQI_type_node))
        return unsigned_intQI_type_node;
      if (bits == TYPE_PRECISION (unsigned_intHI_type_node))
	return unsigned_intHI_type_node;
      if (bits == TYPE_PRECISION (unsigned_intSI_type_node))
	return unsigned_intSI_type_node;
      if (bits == TYPE_PRECISION (unsigned_intDI_type_node))
	return unsigned_intDI_type_node;
      if (bits == TYPE_PRECISION (unsigned_intTI_type_node))
	return unsigned_intTI_type_node;
    }

  return NULL_TREE;
}

/* Return a data type that has machine mode MODE.  If the mode is an
   integer, then UNSIGNEDP selects between signed and unsigned types.  */

tree
gfc_type_for_mode (enum machine_mode mode, int unsignedp)
{
  int i;
  tree *base;

  if (GET_MODE_CLASS (mode) == MODE_FLOAT)
    base = gfc_real_types;
  else if (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
    base = gfc_complex_types;
  else if (SCALAR_INT_MODE_P (mode))
    return gfc_type_for_size (GET_MODE_PRECISION (mode), unsignedp);
  else if (VECTOR_MODE_P (mode))
    {
      enum machine_mode inner_mode = GET_MODE_INNER (mode);
      tree inner_type = gfc_type_for_mode (inner_mode, unsignedp);
      if (inner_type != NULL_TREE)
        return build_vector_type_for_mode (inner_type, mode);
      return NULL_TREE;
    }
  else
    return NULL_TREE;

  for (i = 0; i <= MAX_REAL_KINDS; ++i)
    {
      tree type = base[i];
      if (type && mode == TYPE_MODE (type))
	return type;
    }

  return NULL_TREE;
}

/* Return a type the same as TYPE except unsigned or
   signed according to UNSIGNEDP.  */

tree
gfc_signed_or_unsigned_type (int unsignedp, tree type)
{
  if (TREE_CODE (type) != INTEGER_TYPE || TYPE_UNSIGNED (type) == unsignedp)
    return type;
  else
    return gfc_type_for_size (TYPE_PRECISION (type), unsignedp);
}

/* Return an unsigned type the same as TYPE in other respects.  */

tree
gfc_unsigned_type (tree type)
{
  return gfc_signed_or_unsigned_type (1, type);
}

/* Return a signed type the same as TYPE in other respects.  */

tree
gfc_signed_type (tree type)
{
  return gfc_signed_or_unsigned_type (0, type);
}

#include "gt-fortran-trans-types.h"
