/* Backend support for Fortran 95 basic types and derived types.
   Copyright (C) 2002-2014 Free Software Foundation, Inc.
   Contributed by Paul Brook <paul@nowt.org>
   and Steven Bosscher <s.bosscher@student.tudelft.nl>

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

/* trans-types.c -- gfortran backend types */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"		/* For INTMAX_TYPE, INT8_TYPE, INT16_TYPE, INT32_TYPE,
			   INT64_TYPE, INT_LEAST8_TYPE, INT_LEAST16_TYPE,
			   INT_LEAST32_TYPE, INT_LEAST64_TYPE, INT_FAST8_TYPE,
			   INT_FAST16_TYPE, INT_FAST32_TYPE, INT_FAST64_TYPE,
			   BOOL_TYPE_SIZE, BITS_PER_UNIT, POINTER_SIZE,
			   INT_TYPE_SIZE, CHAR_TYPE_SIZE, SHORT_TYPE_SIZE,
			   LONG_TYPE_SIZE, LONG_LONG_TYPE_SIZE,
			   FLOAT_TYPE_SIZE, DOUBLE_TYPE_SIZE,
			   LONG_DOUBLE_TYPE_SIZE and LIBGCC2_HAS_TF_MODE.  */
#include "tree.h"
#include "stor-layout.h"
#include "stringpool.h"
#include "langhooks.h"	/* For iso-c-bindings.def.  */
#include "target.h"
#include "ggc.h"
#include "diagnostic-core.h"  /* For fatal_error.  */
#include "toplev.h"	/* For rest_of_decl_compilation.  */
#include "gfortran.h"
#include "trans.h"
#include "trans-types.h"
#include "trans-const.h"
#include "flags.h"
#include "dwarf2out.h"	/* For struct array_descr_info.  */


#if (GFC_MAX_DIMENSIONS < 10)
#define GFC_RANK_DIGITS 1
#define GFC_RANK_PRINTF_FORMAT "%01d"
#elif (GFC_MAX_DIMENSIONS < 100)
#define GFC_RANK_DIGITS 2
#define GFC_RANK_PRINTF_FORMAT "%02d"
#else
#error If you really need >99 dimensions, continue the sequence above...
#endif

/* array of structs so we don't have to worry about xmalloc or free */
CInteropKind_t c_interop_kinds_table[ISOCBINDING_NUMBER];

tree gfc_array_index_type;
tree gfc_array_range_type;
tree gfc_character1_type_node;
tree pvoid_type_node;
tree prvoid_type_node;
tree ppvoid_type_node;
tree pchar_type_node;
tree pfunc_type_node;

tree gfc_charlen_type_node;

tree float128_type_node = NULL_TREE;
tree complex_float128_type_node = NULL_TREE;

bool gfc_real16_is_float128 = false;

static GTY(()) tree gfc_desc_dim_type;
static GTY(()) tree gfc_max_array_element_size;
static GTY(()) tree gfc_array_descriptor_base[2 * (GFC_MAX_DIMENSIONS+1)];
static GTY(()) tree gfc_array_descriptor_base_caf[2 * (GFC_MAX_DIMENSIONS+1)];

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

#define MAX_CHARACTER_KINDS 2
gfc_character_info gfc_character_kinds[MAX_CHARACTER_KINDS + 1];
static GTY(()) tree gfc_character_types[MAX_CHARACTER_KINDS + 1];
static GTY(()) tree gfc_pcharacter_types[MAX_CHARACTER_KINDS + 1];

static tree gfc_add_field_to_struct_1 (tree, tree, tree, tree **);

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
int gfc_atomic_int_kind;
int gfc_atomic_logical_kind;

/* The kind size used for record offsets. If the target system supports
   kind=8, this will be set to 8, otherwise it is set to 4.  */
int gfc_intio_kind;

/* The integer kind used to store character lengths.  */
int gfc_charlen_int_kind;

/* The size of the numeric storage unit and character storage unit.  */
int gfc_numeric_storage_size;
int gfc_character_storage_size;


bool
gfc_check_any_c_kind (gfc_typespec *ts)
{
  int i;

  for (i = 0; i < ISOCBINDING_NUMBER; i++)
    {
      /* Check for any C interoperable kind for the given type/kind in ts.
         This can be used after verify_c_interop to make sure that the
         Fortran kind being used exists in at least some form for C.  */
      if (c_interop_kinds_table[i].f90_type == ts->type &&
          c_interop_kinds_table[i].value == ts->kind)
        return true;
    }

  return false;
}


static int
get_real_kind_from_node (tree type)
{
  int i;

  for (i = 0; gfc_real_kinds[i].kind != 0; i++)
    if (gfc_real_kinds[i].mode_precision == TYPE_PRECISION (type))
      return gfc_real_kinds[i].kind;

  return -4;
}

static int
get_int_kind_from_node (tree type)
{
  int i;

  if (!type)
    return -2;

  for (i = 0; gfc_integer_kinds[i].kind != 0; i++)
    if (gfc_integer_kinds[i].bit_size == TYPE_PRECISION (type))
      return gfc_integer_kinds[i].kind;

  return -1;
}

/* Return a typenode for the "standard" C type with a given name.  */
static tree
get_typenode_from_name (const char *name)
{
  if (name == NULL || *name == '\0')
    return NULL_TREE;

  if (strcmp (name, "char") == 0)
    return char_type_node;
  if (strcmp (name, "unsigned char") == 0)
    return unsigned_char_type_node;
  if (strcmp (name, "signed char") == 0)
    return signed_char_type_node;

  if (strcmp (name, "short int") == 0)
    return short_integer_type_node;
  if (strcmp (name, "short unsigned int") == 0)
    return short_unsigned_type_node;

  if (strcmp (name, "int") == 0)
    return integer_type_node;
  if (strcmp (name, "unsigned int") == 0)
    return unsigned_type_node;

  if (strcmp (name, "long int") == 0)
    return long_integer_type_node;
  if (strcmp (name, "long unsigned int") == 0)
    return long_unsigned_type_node;

  if (strcmp (name, "long long int") == 0)
    return long_long_integer_type_node;
  if (strcmp (name, "long long unsigned int") == 0)
    return long_long_unsigned_type_node;

  gcc_unreachable ();
}

static int
get_int_kind_from_name (const char *name)
{
  return get_int_kind_from_node (get_typenode_from_name (name));
}


/* Get the kind number corresponding to an integer of given size,
   following the required return values for ISO_FORTRAN_ENV INT* constants:
   -2 is returned if we support a kind of larger size, -1 otherwise.  */
int
gfc_get_int_kind_from_width_isofortranenv (int size)
{
  int i;

  /* Look for a kind with matching storage size.  */
  for (i = 0; gfc_integer_kinds[i].kind != 0; i++)
    if (gfc_integer_kinds[i].bit_size == size)
      return gfc_integer_kinds[i].kind;

  /* Look for a kind with larger storage size.  */
  for (i = 0; gfc_integer_kinds[i].kind != 0; i++)
    if (gfc_integer_kinds[i].bit_size > size)
      return -2;

  return -1;
}

/* Get the kind number corresponding to a real of given storage size,
   following the required return values for ISO_FORTRAN_ENV REAL* constants:
   -2 is returned if we support a kind of larger size, -1 otherwise.  */
int
gfc_get_real_kind_from_width_isofortranenv (int size)
{
  int i;

  size /= 8;

  /* Look for a kind with matching storage size.  */
  for (i = 0; gfc_real_kinds[i].kind != 0; i++)
    if (int_size_in_bytes (gfc_get_real_type (gfc_real_kinds[i].kind)) == size)
      return gfc_real_kinds[i].kind;

  /* Look for a kind with larger storage size.  */
  for (i = 0; gfc_real_kinds[i].kind != 0; i++)
    if (int_size_in_bytes (gfc_get_real_type (gfc_real_kinds[i].kind)) > size)
      return -2;

  return -1;
}



static int
get_int_kind_from_width (int size)
{
  int i;

  for (i = 0; gfc_integer_kinds[i].kind != 0; i++)
    if (gfc_integer_kinds[i].bit_size == size)
      return gfc_integer_kinds[i].kind;

  return -2;
}

static int
get_int_kind_from_minimal_width (int size)
{
  int i;

  for (i = 0; gfc_integer_kinds[i].kind != 0; i++)
    if (gfc_integer_kinds[i].bit_size >= size)
      return gfc_integer_kinds[i].kind;

  return -2;
}


/* Generate the CInteropKind_t objects for the C interoperable
   kinds.  */

void
gfc_init_c_interop_kinds (void)
{
  int i;

  /* init all pointers in the list to NULL */
  for (i = 0; i < ISOCBINDING_NUMBER; i++)
    {
      /* Initialize the name and value fields.  */
      c_interop_kinds_table[i].name[0] = '\0';
      c_interop_kinds_table[i].value = -100;
      c_interop_kinds_table[i].f90_type = BT_UNKNOWN;
    }

#define NAMED_INTCST(a,b,c,d) \
  strncpy (c_interop_kinds_table[a].name, b, strlen(b) + 1); \
  c_interop_kinds_table[a].f90_type = BT_INTEGER; \
  c_interop_kinds_table[a].value = c;
#define NAMED_REALCST(a,b,c,d) \
  strncpy (c_interop_kinds_table[a].name, b, strlen(b) + 1); \
  c_interop_kinds_table[a].f90_type = BT_REAL; \
  c_interop_kinds_table[a].value = c;
#define NAMED_CMPXCST(a,b,c,d) \
  strncpy (c_interop_kinds_table[a].name, b, strlen(b) + 1); \
  c_interop_kinds_table[a].f90_type = BT_COMPLEX; \
  c_interop_kinds_table[a].value = c;
#define NAMED_LOGCST(a,b,c) \
  strncpy (c_interop_kinds_table[a].name, b, strlen(b) + 1); \
  c_interop_kinds_table[a].f90_type = BT_LOGICAL; \
  c_interop_kinds_table[a].value = c;
#define NAMED_CHARKNDCST(a,b,c) \
  strncpy (c_interop_kinds_table[a].name, b, strlen(b) + 1); \
  c_interop_kinds_table[a].f90_type = BT_CHARACTER; \
  c_interop_kinds_table[a].value = c;
#define NAMED_CHARCST(a,b,c) \
  strncpy (c_interop_kinds_table[a].name, b, strlen(b) + 1); \
  c_interop_kinds_table[a].f90_type = BT_CHARACTER; \
  c_interop_kinds_table[a].value = c;
#define DERIVED_TYPE(a,b,c) \
  strncpy (c_interop_kinds_table[a].name, b, strlen(b) + 1); \
  c_interop_kinds_table[a].f90_type = BT_DERIVED; \
  c_interop_kinds_table[a].value = c;
#define NAMED_FUNCTION(a,b,c,d) \
  strncpy (c_interop_kinds_table[a].name, b, strlen(b) + 1); \
  c_interop_kinds_table[a].f90_type = BT_PROCEDURE; \
  c_interop_kinds_table[a].value = c;
#define NAMED_SUBROUTINE(a,b,c,d) \
  strncpy (c_interop_kinds_table[a].name, b, strlen(b) + 1); \
  c_interop_kinds_table[a].f90_type = BT_PROCEDURE; \
  c_interop_kinds_table[a].value = c;
#include "iso-c-binding.def"
}


/* Query the target to determine which machine modes are available for
   computation.  Choose KIND numbers for them.  */

void
gfc_init_kinds (void)
{
  unsigned int mode;
  int i_index, r_index, kind;
  bool saw_i4 = false, saw_i8 = false;
  bool saw_r4 = false, saw_r8 = false, saw_r10 = false, saw_r16 = false;

  for (i_index = 0, mode = MIN_MODE_INT; mode <= MAX_MODE_INT; mode++)
    {
      int kind, bitsize;

      if (!targetm.scalar_mode_supported_p ((enum machine_mode) mode))
	continue;

      /* The middle end doesn't support constants larger than 2*HWI.
	 Perhaps the target hook shouldn't have accepted these either,
	 but just to be safe...  */
      bitsize = GET_MODE_BITSIZE ((enum machine_mode) mode);
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

  /* Set the kind used to match GFC_INT_IO in libgfortran.  This is
     used for large file access.  */

  if (saw_i8)
    gfc_intio_kind = 8;
  else
    gfc_intio_kind = 4;

  /* If we do not at least have kind = 4, everything is pointless.  */
  gcc_assert(saw_i4);

  /* Set the maximum integer kind.  Used with at least BOZ constants.  */
  gfc_max_integer_kind = gfc_integer_kinds[i_index - 1].kind;

  for (r_index = 0, mode = MIN_MODE_FLOAT; mode <= MAX_MODE_FLOAT; mode++)
    {
      const struct real_format *fmt =
	REAL_MODE_FORMAT ((enum machine_mode) mode);
      int kind;

      if (fmt == NULL)
	continue;
      if (!targetm.scalar_mode_supported_p ((enum machine_mode) mode))
	continue;

      /* Only let float, double, long double and __float128 go through.
	 Runtime support for others is not provided, so they would be
	 useless.  */
	if (mode != TYPE_MODE (float_type_node)
	    && (mode != TYPE_MODE (double_type_node))
	    && (mode != TYPE_MODE (long_double_type_node))
#if defined(LIBGCC2_HAS_TF_MODE) && defined(ENABLE_LIBQUADMATH_SUPPORT)
	    && (mode != TFmode)
#endif
	   )
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
      if (kind == 10)
	saw_r10 = true;
      if (kind == 16)
	saw_r16 = true;

      /* Careful we don't stumble a weird internal mode.  */
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

  /* Choose the default integer kind.  We choose 4 unless the user directs us
     otherwise.  Even if the user specified that the default integer kind is 8,
     the numeric storage size is not 64 bits.  In this case, a warning will be
     issued when NUMERIC_STORAGE_SIZE is used.  Set NUMERIC_STORAGE_SIZE to 32.  */

  gfc_numeric_storage_size = 4 * 8;

  if (gfc_option.flag_default_integer)
    {
      if (!saw_i8)
	fatal_error ("INTEGER(KIND=8) is not available for -fdefault-integer-8 option");

      gfc_default_integer_kind = 8;

    }
  else if (gfc_option.flag_integer4_kind == 8)
    {
      if (!saw_i8)
	fatal_error ("INTEGER(KIND=8) is not available for -finteger-4-integer-8 option");

      gfc_default_integer_kind = 8;
    }
  else if (saw_i4)
    {
      gfc_default_integer_kind = 4;
    }
  else
    {
      gfc_default_integer_kind = gfc_integer_kinds[i_index - 1].kind;
      gfc_numeric_storage_size = gfc_integer_kinds[i_index - 1].bit_size;
    }

  /* Choose the default real kind.  Again, we choose 4 when possible.  */
  if (gfc_option.flag_default_real)
    {
      if (!saw_r8)
	fatal_error ("REAL(KIND=8) is not available for -fdefault-real-8 option");

      gfc_default_real_kind = 8;
    }
  else if (gfc_option.flag_real4_kind == 8)
  {
    if (!saw_r8)
      fatal_error ("REAL(KIND=8) is not available for -freal-4-real-8 option");

    gfc_default_real_kind = 8;
  }
  else if (gfc_option.flag_real4_kind == 10)
  {
    if (!saw_r10)
      fatal_error ("REAL(KIND=10) is not available for -freal-4-real-10 option");

    gfc_default_real_kind = 10;
  }
  else if (gfc_option.flag_real4_kind == 16)
  {
    if (!saw_r16)
      fatal_error ("REAL(KIND=16) is not available for -freal-4-real-16 option");

    gfc_default_real_kind = 16;
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
  else if (gfc_option.flag_real8_kind == 4)
    {
      if (!saw_r4)
	fatal_error ("REAL(KIND=4) is not available for -freal-8-real-4 option");

	gfc_default_double_kind = 4;
    }
  else if (gfc_option.flag_real8_kind == 10 )
    {
      if (!saw_r10)
	fatal_error ("REAL(KIND=10) is not available for -freal-8-real-10 option");

	gfc_default_double_kind = 10;
    }
  else if (gfc_option.flag_real8_kind == 16 )
    {
      if (!saw_r16)
	fatal_error ("REAL(KIND=10) is not available for -freal-8-real-16 option");

	gfc_default_double_kind = 16;
    }
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

  /* We only have two character kinds: ASCII and UCS-4.
     ASCII corresponds to a 8-bit integer type, if one is available.
     UCS-4 corresponds to a 32-bit integer type, if one is available. */
  i_index = 0;
  if ((kind = get_int_kind_from_width (8)) > 0)
    {
      gfc_character_kinds[i_index].kind = kind;
      gfc_character_kinds[i_index].bit_size = 8;
      gfc_character_kinds[i_index].name = "ascii";
      i_index++;
    }
  if ((kind = get_int_kind_from_width (32)) > 0)
    {
      gfc_character_kinds[i_index].kind = kind;
      gfc_character_kinds[i_index].bit_size = 32;
      gfc_character_kinds[i_index].name = "iso_10646";
      i_index++;
    }

  /* Choose the smallest integer kind for our default character.  */
  gfc_default_character_kind = gfc_character_kinds[0].kind;
  gfc_character_storage_size = gfc_default_character_kind * 8;

  gfc_index_integer_kind = get_int_kind_from_name (PTRDIFF_TYPE);

  /* Pick a kind the same size as the C "int" type.  */
  gfc_c_int_kind = INT_TYPE_SIZE / 8;

  /* Choose atomic kinds to match C's int.  */
  gfc_atomic_int_kind = gfc_c_int_kind;
  gfc_atomic_logical_kind = gfc_c_int_kind;
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
  int i;

  for (i = 0; gfc_character_kinds[i].kind; i++)
    if (gfc_character_kinds[i].kind == kind)
      return i;

  return -1;
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

tree
gfc_build_uint_type (int size)
{
  if (size == CHAR_TYPE_SIZE)
    return unsigned_char_type_node;
  if (size == SHORT_TYPE_SIZE)
    return short_unsigned_type_node;
  if (size == INT_TYPE_SIZE)
    return unsigned_type_node;
  if (size == LONG_TYPE_SIZE)
    return long_unsigned_type_node;
  if (size == LONG_LONG_TYPE_SIZE)
    return long_long_unsigned_type_node;

  return make_unsigned_type (size);
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
  if (mode_precision != LONG_DOUBLE_TYPE_SIZE && mode_precision == 128)
    {
      info->c_float128 = 1;
      gfc_real16_is_float128 = true;
    }

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


/* Create the backend type nodes. We map them to their
   equivalent C type, at least for now.  We also give
   names to the types here, and we push them in the
   global binding level context.*/

void
gfc_init_types (void)
{
  char name_buf[18];
  int index;
  tree type;
  unsigned n;

  /* Create and name the types.  */
#define PUSH_TYPE(name, node) \
  pushdecl (build_decl (input_location, \
			TYPE_DECL, get_identifier (name), node))

  for (index = 0; gfc_integer_kinds[index].kind != 0; ++index)
    {
      type = gfc_build_int_type (&gfc_integer_kinds[index]);
      /* Ensure integer(kind=1) doesn't have TYPE_STRING_FLAG set.  */
      if (TYPE_STRING_FLAG (type))
	type = make_signed_type (gfc_integer_kinds[index].bit_size);
      gfc_integer_types[index] = type;
      snprintf (name_buf, sizeof(name_buf), "integer(kind=%d)",
		gfc_integer_kinds[index].kind);
      PUSH_TYPE (name_buf, type);
    }

  for (index = 0; gfc_logical_kinds[index].kind != 0; ++index)
    {
      type = gfc_build_logical_type (&gfc_logical_kinds[index]);
      gfc_logical_types[index] = type;
      snprintf (name_buf, sizeof(name_buf), "logical(kind=%d)",
		gfc_logical_kinds[index].kind);
      PUSH_TYPE (name_buf, type);
    }

  for (index = 0; gfc_real_kinds[index].kind != 0; index++)
    {
      type = gfc_build_real_type (&gfc_real_kinds[index]);
      gfc_real_types[index] = type;
      snprintf (name_buf, sizeof(name_buf), "real(kind=%d)",
		gfc_real_kinds[index].kind);
      PUSH_TYPE (name_buf, type);

      if (gfc_real_kinds[index].c_float128)
	float128_type_node = type;

      type = gfc_build_complex_type (type);
      gfc_complex_types[index] = type;
      snprintf (name_buf, sizeof(name_buf), "complex(kind=%d)",
		gfc_real_kinds[index].kind);
      PUSH_TYPE (name_buf, type);

      if (gfc_real_kinds[index].c_float128)
	complex_float128_type_node = type;
    }

  for (index = 0; gfc_character_kinds[index].kind != 0; ++index)
    {
      type = gfc_build_uint_type (gfc_character_kinds[index].bit_size);
      type = build_qualified_type (type, TYPE_UNQUALIFIED);
      snprintf (name_buf, sizeof(name_buf), "character(kind=%d)",
		gfc_character_kinds[index].kind);
      PUSH_TYPE (name_buf, type);
      gfc_character_types[index] = type;
      gfc_pcharacter_types[index] = build_pointer_type (type);
    }
  gfc_character1_type_node = gfc_character_types[0];

  PUSH_TYPE ("byte", unsigned_char_type_node);
  PUSH_TYPE ("void", void_type_node);

  /* DBX debugging output gets upset if these aren't set.  */
  if (!TYPE_NAME (integer_type_node))
    PUSH_TYPE ("c_integer", integer_type_node);
  if (!TYPE_NAME (char_type_node))
    PUSH_TYPE ("c_char", char_type_node);

#undef PUSH_TYPE

  pvoid_type_node = build_pointer_type (void_type_node);
  prvoid_type_node = build_qualified_type (pvoid_type_node, TYPE_QUAL_RESTRICT);
  ppvoid_type_node = build_pointer_type (pvoid_type_node);
  pchar_type_node = build_pointer_type (gfc_character1_type_node);
  pfunc_type_node
    = build_pointer_type (build_function_type_list (void_type_node, NULL_TREE));

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
  gfc_max_array_element_size
    = wide_int_to_tree (long_unsigned_type_node,
			wi::mask (n, UNSIGNED,
				  TYPE_PRECISION (long_unsigned_type_node)));

  boolean_type_node = gfc_get_logical_type (gfc_default_logical_kind);
  boolean_true_node = build_int_cst (boolean_type_node, 1);
  boolean_false_node = build_int_cst (boolean_type_node, 0);

  /* ??? Shouldn't this be based on gfc_index_integer_kind or so?  */
  gfc_charlen_int_kind = 4;
  gfc_charlen_type_node = gfc_get_int_type (gfc_charlen_int_kind);
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

tree
gfc_get_char_type (int kind)
{
  int index = gfc_validate_kind (BT_CHARACTER, kind, true);
  return index < 0 ? 0 : gfc_character_types[index];
}

tree
gfc_get_pchar_type (int kind)
{
  int index = gfc_validate_kind (BT_CHARACTER, kind, true);
  return index < 0 ? 0 : gfc_pcharacter_types[index];
}


/* Create a character type with the given kind and length.  */

tree
gfc_get_character_type_len_for_eltype (tree eltype, tree len)
{
  tree bounds, type;

  bounds = build_range_type (gfc_charlen_type_node, gfc_index_one_node, len);
  type = build_array_type (eltype, bounds);
  TYPE_STRING_FLAG (type) = 1;

  return type;
}

tree
gfc_get_character_type_len (int kind, tree len)
{
  gfc_validate_kind (BT_CHARACTER, kind, false);
  return gfc_get_character_type_len_for_eltype (gfc_get_char_type (kind), len);
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
      /* We use INTEGER(c_intptr_t) for C_PTR and C_FUNPTR once the symbol
         has been resolved.  This is done so we can convert C_PTR and
         C_FUNPTR to simple variables that get translated to (void *).  */
      if (spec->f90_type == BT_VOID)
	{
	  if (spec->u.derived
	      && spec->u.derived->intmod_sym_id == ISOCBINDING_PTR)
	    basetype = ptr_type_node;
	  else
	    basetype = pfunc_type_node;
	}
      else
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
#if 0
      if (spec->deferred)
	basetype = gfc_get_character_type (spec->kind, NULL);
      else
#endif
	basetype = gfc_get_character_type (spec->kind, spec->u.cl);
      break;

    case BT_HOLLERITH:
      /* Since this cannot be used, return a length one character.  */
      basetype = gfc_get_character_type_len (gfc_default_character_kind,
					     gfc_index_one_node);
      break;

    case BT_DERIVED:
    case BT_CLASS:
      basetype = gfc_get_derived_type (spec->u.derived);

      if (spec->type == BT_CLASS)
	GFC_CLASS_TYPE_P (basetype) = 1;

      /* If we're dealing with either C_PTR or C_FUNPTR, we modified the
         type and kind to fit a (void *) and the basetype returned was a
         ptr_type_node.  We need to pass up this new information to the
         symbol that was declared of type C_PTR or C_FUNPTR.  */
      if (spec->u.derived->ts.f90_type == BT_VOID)
        {
          spec->type = BT_INTEGER;
          spec->kind = gfc_index_integer_kind;
          spec->f90_type = BT_VOID;
        }
      break;
    case BT_VOID:
    case BT_ASSUMED:
      /* This is for the second arg to c_f_pointer and c_f_procpointer
         of the iso_c_binding module, to accept any ptr type.  */
      basetype = ptr_type_node;
      if (spec->f90_type == BT_VOID)
	{
	  if (spec->u.derived
	      && spec->u.derived->intmod_sym_id == ISOCBINDING_PTR)
	    basetype = ptr_type_node;
	  else
	    basetype = pfunc_type_node;
	}
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
      if (GFC_TYPE_ARRAY_RANK (type) == 0)
	{
	  gcc_assert (GFC_TYPE_ARRAY_CORANK (type) > 0);
	  element = type;
	}
      else
	{
	  gcc_assert (TREE_CODE (type) == ARRAY_TYPE);
	  element = TREE_TYPE (type);
	}
    }
  else
    {
      gcc_assert (GFC_DESCRIPTOR_TYPE_P (type));
      element = GFC_TYPE_ARRAY_DATAPTR_TYPE (type);

      gcc_assert (TREE_CODE (element) == POINTER_TYPE);
      element = TREE_TYPE (element);

      /* For arrays, which are not scalar coarrays.  */
      if (TREE_CODE (element) == ARRAY_TYPE && !TYPE_STRING_FLAG (element))
	element = TREE_TYPE (element);
    }

  return element;
}

/* Build an array.  This function is called from gfc_sym_type().
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

   Translation code should use gfc_conv_descriptor_* rather than
   accessing the descriptor directly.  Any changes to the array
   descriptor type will require changes in gfc_conv_descriptor_* and
   gfc_build_array_initializer.

   This is represented internally as a RECORD_TYPE. The index nodes
   are gfc_array_index_type and the data node is a pointer to the
   data.  See below for the handling of character types.

   The dtype member is formatted as follows:
    rank = dtype & GFC_DTYPE_RANK_MASK // 3 bits
    type = (dtype & GFC_DTYPE_TYPE_MASK) >> GFC_DTYPE_TYPE_SHIFT // 3 bits
    size = dtype >> GFC_DTYPE_SIZE_SHIFT

   I originally used nested ARRAY_TYPE nodes to represent arrays, but
   this generated poor code for assumed/deferred size arrays.  These
   require use of PLACEHOLDER_EXPR/WITH_RECORD_EXPR, which isn't part
   of the GENERIC grammar.  Also, there is no way to explicitly set
   the array stride, so all data must be packed(1).  I've tried to
   mark all the functions which would require modification with a GCC
   ARRAYS comment.

   The data component points to the first element in the array.  The
   offset field is the position of the origin of the array (i.e. element
   (0, 0 ...)).  This may be outside the bounds of the array.

   An element is accessed by
    data[offset + index0*stride0 + index1*stride1 + index2*stride2]
   This gives good performance as the computation does not involve the
   bounds of the array.  For packed arrays, this is optimized further
   by substituting the known strides.

   This system has one problem: all array bounds must be within 2^31
   elements of the origin (2^63 on 64-bit machines).  For example
    integer, dimension (80000:90000, 80000:90000, 2) :: array
   may not work properly on 32-bit machines because 80000*80000 >
   2^31, so the calculation for stride2 would overflow.  This may
   still work, but I haven't checked, and it relies on the overflow
   doing the right thing.

   The way to fix this problem is to access elements as follows:
    data[(index0-lbound0)*stride0 + (index1-lbound1)*stride1]
   Obviously this is much slower.  I will make this a compile time
   option, something like -fsmall-array-offsets.  Mixing code compiled
   with and without this switch will work.

   (1) This can be worked around by modifying the upper bound of the
   previous dimension.  This requires extra fields in the descriptor
   (both real_ubound and fake_ubound).  */


/* Returns true if the array sym does not require a descriptor.  */

int
gfc_is_nodesc_array (gfc_symbol * sym)
{
  gcc_assert (sym->attr.dimension || sym->attr.codimension);

  /* We only want local arrays.  */
  if (sym->attr.pointer || sym->attr.allocatable)
    return 0;

  /* We want a descriptor for associate-name arrays that do not have an
     explicitly known shape already.  */
  if (sym->assoc && sym->as->type != AS_EXPLICIT)
    return 0;

  if (sym->attr.dummy)
    return sym->as->type != AS_ASSUMED_SHAPE
	   && sym->as->type != AS_ASSUMED_RANK;

  if (sym->attr.result || sym->attr.function)
    return 0;

  gcc_assert (sym->as->type == AS_EXPLICIT || sym->as->cp_was_assumed);

  return 1;
}


/* Create an array descriptor type.  */

static tree
gfc_build_array_type (tree type, gfc_array_spec * as,
		      enum gfc_array_kind akind, bool restricted,
		      bool contiguous)
{
  tree lbound[GFC_MAX_DIMENSIONS];
  tree ubound[GFC_MAX_DIMENSIONS];
  int n;

  if (as->type == AS_ASSUMED_RANK)
    for (n = 0; n < GFC_MAX_DIMENSIONS; n++)
      {
	lbound[n] = NULL_TREE;
	ubound[n] = NULL_TREE;
      }

  for (n = 0; n < as->rank; n++)
    {
      /* Create expressions for the known bounds of the array.  */
      if (as->type == AS_ASSUMED_SHAPE && as->lower[n] == NULL)
        lbound[n] = gfc_index_one_node;
      else
        lbound[n] = gfc_conv_array_bound (as->lower[n]);
      ubound[n] = gfc_conv_array_bound (as->upper[n]);
    }

  for (n = as->rank; n < as->rank + as->corank; n++)
    {
      if (as->type != AS_DEFERRED && as->lower[n] == NULL)
        lbound[n] = gfc_index_one_node;
      else
        lbound[n] = gfc_conv_array_bound (as->lower[n]);

      if (n < as->rank + as->corank - 1)
	ubound[n] = gfc_conv_array_bound (as->upper[n]);
    }

  if (as->type == AS_ASSUMED_SHAPE)
    akind = contiguous ? GFC_ARRAY_ASSUMED_SHAPE_CONT
		       : GFC_ARRAY_ASSUMED_SHAPE;
  else if (as->type == AS_ASSUMED_RANK)
    akind = contiguous ? GFC_ARRAY_ASSUMED_RANK_CONT
		       : GFC_ARRAY_ASSUMED_RANK;
  return gfc_get_array_type_bounds (type, as->rank == -1
					  ? GFC_MAX_DIMENSIONS : as->rank,
				    as->corank, lbound,
				    ubound, 0, akind, restricted);
}

/* Returns the struct descriptor_dimension type.  */

static tree
gfc_get_desc_dim_type (void)
{
  tree type;
  tree decl, *chain = NULL;

  if (gfc_desc_dim_type)
    return gfc_desc_dim_type;

  /* Build the type node.  */
  type = make_node (RECORD_TYPE);

  TYPE_NAME (type) = get_identifier ("descriptor_dimension");
  TYPE_PACKED (type) = 1;

  /* Consists of the stride, lbound and ubound members.  */
  decl = gfc_add_field_to_struct_1 (type,
				    get_identifier ("stride"),
				    gfc_array_index_type, &chain);
  TREE_NO_WARNING (decl) = 1;

  decl = gfc_add_field_to_struct_1 (type,
				    get_identifier ("lbound"),
				    gfc_array_index_type, &chain);
  TREE_NO_WARNING (decl) = 1;

  decl = gfc_add_field_to_struct_1 (type,
				    get_identifier ("ubound"),
				    gfc_array_index_type, &chain);
  TREE_NO_WARNING (decl) = 1;

  /* Finish off the type.  */
  gfc_finish_type (type);
  TYPE_DECL_SUPPRESS_DEBUG (TYPE_STUB_DECL (type)) = 1;

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
      n = BT_INTEGER;
      break;

    case BOOLEAN_TYPE:
      n = BT_LOGICAL;
      break;

    case REAL_TYPE:
      n = BT_REAL;
      break;

    case COMPLEX_TYPE:
      n = BT_COMPLEX;
      break;

    /* We will never have arrays of arrays.  */
    case RECORD_TYPE:
      n = BT_DERIVED;
      break;

    case ARRAY_TYPE:
      n = BT_CHARACTER;
      break;

    case POINTER_TYPE:
      n = BT_ASSUMED;
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
	gfc_fatal_error ("Array element size too big at %C");

      i += TREE_INT_CST_LOW (size) << GFC_DTYPE_SIZE_SHIFT;
    }
  dtype = build_int_cst (gfc_array_index_type, i);

  if (size && !INTEGER_CST_P (size))
    {
      tmp = build_int_cst (gfc_array_index_type, GFC_DTYPE_SIZE_SHIFT);
      tmp  = fold_build2_loc (input_location, LSHIFT_EXPR,
			      gfc_array_index_type,
			      fold_convert (gfc_array_index_type, size), tmp);
      dtype = fold_build2_loc (input_location, PLUS_EXPR, gfc_array_index_type,
			       tmp, dtype);
    }
  /* If we don't know the size we leave it as zero.  This should never happen
     for anything that is actually used.  */
  /* TODO: Check this is actually true, particularly when repacking
     assumed size parameters.  */

  GFC_TYPE_ARRAY_DTYPE (type) = dtype;
  return dtype;
}


/* Build an array type for use without a descriptor, packed according
   to the value of PACKED.  */

tree
gfc_get_nodesc_array_type (tree etype, gfc_array_spec * as, gfc_packed packed,
			   bool restricted)
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
  if (as->rank)
    type = make_node (ARRAY_TYPE);
  else
    type = build_variant_type_copy (etype);

  GFC_ARRAY_TYPE_P (type) = 1;
  TYPE_LANG_SPECIFIC (type)
      = ggc_alloc_cleared_lang_type (sizeof (struct lang_type));

  known_stride = (packed != PACKED_NO);
  known_offset = 1;
  for (n = 0; n < as->rank; n++)
    {
      /* Fill in the stride and bound components of the type.  */
      if (known_stride)
	tmp = gfc_conv_mpz_to_tree (stride, gfc_index_integer_kind);
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
      if (packed == PACKED_NO || packed == PACKED_PARTIAL)
        known_stride = 0;
    }
  for (n = as->rank; n < as->rank + as->corank; n++)
    {
      expr = as->lower[n];
      if (expr->expr_type == EXPR_CONSTANT)
	tmp = gfc_conv_mpz_to_tree (expr->value.integer,
				    gfc_index_integer_kind);
      else
      	tmp = NULL_TREE;
      GFC_TYPE_ARRAY_LBOUND (type, n) = tmp;

      expr = as->upper[n];
      if (expr && expr->expr_type == EXPR_CONSTANT)
	tmp = gfc_conv_mpz_to_tree (expr->value.integer,
				    gfc_index_integer_kind);
      else
 	tmp = NULL_TREE;
      if (n < as->rank + as->corank - 1)
      GFC_TYPE_ARRAY_UBOUND (type, n) = tmp;
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
  GFC_TYPE_ARRAY_CORANK (type) = as->corank;
  GFC_TYPE_ARRAY_DTYPE (type) = NULL_TREE;
  range = build_range_type (gfc_array_index_type, gfc_index_zero_node,
			    NULL_TREE);
  /* TODO: use main type if it is unbounded.  */
  GFC_TYPE_ARRAY_DATAPTR_TYPE (type) =
    build_pointer_type (build_array_type (etype, range));
  if (restricted)
    GFC_TYPE_ARRAY_DATAPTR_TYPE (type) =
      build_qualified_type (GFC_TYPE_ARRAY_DATAPTR_TYPE (type),
			    TYPE_QUAL_RESTRICT);

  if (as->rank == 0)
    {
      if (packed != PACKED_STATIC  || gfc_option.coarray == GFC_FCOARRAY_LIB)
	{
	  type = build_pointer_type (type);

	  if (restricted)
	    type = build_qualified_type (type, TYPE_QUAL_RESTRICT);

	  GFC_ARRAY_TYPE_P (type) = 1;
	  TYPE_LANG_SPECIFIC (type) = TYPE_LANG_SPECIFIC (TREE_TYPE (type));
	}

      return type;
    }

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

  /* Represent packed arrays as multi-dimensional if they have rank >
     1 and with proper bounds, instead of flat arrays.  This makes for
     better debug info.  */
  if (known_offset)
    {
      tree gtype = etype, rtype, type_decl;

      for (n = as->rank - 1; n >= 0; n--)
	{
	  rtype = build_range_type (gfc_array_index_type,
				    GFC_TYPE_ARRAY_LBOUND (type, n),
				    GFC_TYPE_ARRAY_UBOUND (type, n));
	  gtype = build_array_type (gtype, rtype);
	}
      TYPE_NAME (type) = type_decl = build_decl (input_location,
						 TYPE_DECL, NULL, gtype);
      DECL_ORIGINAL_TYPE (type_decl) = gtype;
    }

  if (packed != PACKED_STATIC || !known_stride
      || (as->corank && gfc_option.coarray == GFC_FCOARRAY_LIB))
    {
      /* For dummy arrays and automatic (heap allocated) arrays we
	 want a pointer to the array.  */
      type = build_pointer_type (type);
      if (restricted)
	type = build_qualified_type (type, TYPE_QUAL_RESTRICT);
      GFC_ARRAY_TYPE_P (type) = 1;
      TYPE_LANG_SPECIFIC (type) = TYPE_LANG_SPECIFIC (TREE_TYPE (type));
    }
  return type;
}


/* Return or create the base type for an array descriptor.  */

static tree
gfc_get_array_descriptor_base (int dimen, int codimen, bool restricted,
			       enum gfc_array_kind akind)
{
  tree fat_type, decl, arraytype, *chain = NULL;
  char name[16 + 2*GFC_RANK_DIGITS + 1 + 1];
  int idx;

  /* Assumed-rank array.  */
  if (dimen == -1)
    dimen = GFC_MAX_DIMENSIONS;

  idx = 2 * (codimen + dimen) + restricted;

  gcc_assert (codimen + dimen >= 0 && codimen + dimen <= GFC_MAX_DIMENSIONS);

  if (gfc_option.coarray == GFC_FCOARRAY_LIB && codimen)
    {
      if (gfc_array_descriptor_base_caf[idx])
	return gfc_array_descriptor_base_caf[idx];
    }
  else if (gfc_array_descriptor_base[idx])
    return gfc_array_descriptor_base[idx];

  /* Build the type node.  */
  fat_type = make_node (RECORD_TYPE);

  sprintf (name, "array_descriptor" GFC_RANK_PRINTF_FORMAT, dimen + codimen);
  TYPE_NAME (fat_type) = get_identifier (name);
  TYPE_NAMELESS (fat_type) = 1;

  /* Add the data member as the first element of the descriptor.  */
  decl = gfc_add_field_to_struct_1 (fat_type,
				    get_identifier ("data"),
				    (restricted
				     ? prvoid_type_node
				     : ptr_type_node), &chain);

  /* Add the base component.  */
  decl = gfc_add_field_to_struct_1 (fat_type,
				    get_identifier ("offset"),
				    gfc_array_index_type, &chain);
  TREE_NO_WARNING (decl) = 1;

  /* Add the dtype component.  */
  decl = gfc_add_field_to_struct_1 (fat_type,
				    get_identifier ("dtype"),
				    gfc_array_index_type, &chain);
  TREE_NO_WARNING (decl) = 1;

  /* Build the array type for the stride and bound components.  */
  if (dimen + codimen > 0)
    {
      arraytype =
	build_array_type (gfc_get_desc_dim_type (),
			  build_range_type (gfc_array_index_type,
					    gfc_index_zero_node,
					    gfc_rank_cst[codimen + dimen - 1]));

      decl = gfc_add_field_to_struct_1 (fat_type, get_identifier ("dim"),
					arraytype, &chain);
      TREE_NO_WARNING (decl) = 1;
    }

  if (gfc_option.coarray == GFC_FCOARRAY_LIB && codimen
      && akind == GFC_ARRAY_ALLOCATABLE)
    {
      decl = gfc_add_field_to_struct_1 (fat_type,
					get_identifier ("token"),
					prvoid_type_node, &chain);
      TREE_NO_WARNING (decl) = 1;
    }

  /* Finish off the type.  */
  gfc_finish_type (fat_type);
  TYPE_DECL_SUPPRESS_DEBUG (TYPE_STUB_DECL (fat_type)) = 1;

  if (gfc_option.coarray == GFC_FCOARRAY_LIB && codimen
      && akind == GFC_ARRAY_ALLOCATABLE)
    gfc_array_descriptor_base_caf[idx] = fat_type;
  else
    gfc_array_descriptor_base[idx] = fat_type;

  return fat_type;
}


/* Build an array (descriptor) type with given bounds.  */

tree
gfc_get_array_type_bounds (tree etype, int dimen, int codimen, tree * lbound,
			   tree * ubound, int packed,
			   enum gfc_array_kind akind, bool restricted)
{
  char name[8 + 2*GFC_RANK_DIGITS + 1 + GFC_MAX_SYMBOL_LEN];
  tree fat_type, base_type, arraytype, lower, upper, stride, tmp, rtype;
  const char *type_name;
  int n;

  base_type = gfc_get_array_descriptor_base (dimen, codimen, restricted, akind);
  fat_type = build_distinct_type_copy (base_type);
  /* Make sure that nontarget and target array type have the same canonical
     type (and same stub decl for debug info).  */
  base_type = gfc_get_array_descriptor_base (dimen, codimen, false, akind);
  TYPE_CANONICAL (fat_type) = base_type;
  TYPE_STUB_DECL (fat_type) = TYPE_STUB_DECL (base_type);

  tmp = TYPE_NAME (etype);
  if (tmp && TREE_CODE (tmp) == TYPE_DECL)
    tmp = DECL_NAME (tmp);
  if (tmp)
    type_name = IDENTIFIER_POINTER (tmp);
  else
    type_name = "unknown";
  sprintf (name, "array" GFC_RANK_PRINTF_FORMAT "_%.*s", dimen + codimen,
	   GFC_MAX_SYMBOL_LEN, type_name);
  TYPE_NAME (fat_type) = get_identifier (name);
  TYPE_NAMELESS (fat_type) = 1;

  GFC_DESCRIPTOR_TYPE_P (fat_type) = 1;
  TYPE_LANG_SPECIFIC (fat_type)
    = ggc_alloc_cleared_lang_type (sizeof (struct lang_type));

  GFC_TYPE_ARRAY_RANK (fat_type) = dimen;
  GFC_TYPE_ARRAY_CORANK (fat_type) = codimen;
  GFC_TYPE_ARRAY_DTYPE (fat_type) = NULL_TREE;
  GFC_TYPE_ARRAY_AKIND (fat_type) = akind;

  /* Build an array descriptor record type.  */
  if (packed != 0)
    stride = gfc_index_one_node;
  else
    stride = NULL_TREE;
  for (n = 0; n < dimen + codimen; n++)
    {
      if (n < dimen)
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

      if (codimen && n == dimen + codimen - 1)
	break;

      upper = ubound[n];
      if (upper != NULL_TREE)
	{
	  if (INTEGER_CST_P (upper))
	    GFC_TYPE_ARRAY_UBOUND (fat_type, n) = upper;
	  else
	    upper = NULL_TREE;
	}

      if (n >= dimen)
	continue;

      if (upper != NULL_TREE && lower != NULL_TREE && stride != NULL_TREE)
	{
	  tmp = fold_build2_loc (input_location, MINUS_EXPR,
				 gfc_array_index_type, upper, lower);
	  tmp = fold_build2_loc (input_location, PLUS_EXPR,
				 gfc_array_index_type, tmp,
				 gfc_index_one_node);
	  stride = fold_build2_loc (input_location, MULT_EXPR,
				    gfc_array_index_type, tmp, stride);
	  /* Check the folding worked.  */
	  gcc_assert (INTEGER_CST_P (stride));
	}
      else
	stride = NULL_TREE;
    }
  GFC_TYPE_ARRAY_SIZE (fat_type) = stride;

  /* TODO: known offsets for descriptors.  */
  GFC_TYPE_ARRAY_OFFSET (fat_type) = NULL_TREE;

  if (dimen == 0)
    {
      arraytype =  build_pointer_type (etype);
      if (restricted)
	arraytype = build_qualified_type (arraytype, TYPE_QUAL_RESTRICT);

      GFC_TYPE_ARRAY_DATAPTR_TYPE (fat_type) = arraytype;
      return fat_type;
    }

  /* We define data as an array with the correct size if possible.
     Much better than doing pointer arithmetic.  */
  if (stride)
    rtype = build_range_type (gfc_array_index_type, gfc_index_zero_node,
			      int_const_binop (MINUS_EXPR, stride,
					       build_int_cst (TREE_TYPE (stride), 1)));
  else
    rtype = gfc_array_range_type;
  arraytype = build_array_type (etype, rtype);
  arraytype = build_pointer_type (arraytype);
  if (restricted)
    arraytype = build_qualified_type (arraytype, TYPE_QUAL_RESTRICT);
  GFC_TYPE_ARRAY_DATAPTR_TYPE (fat_type) = arraytype;

  /* This will generate the base declarations we need to emit debug
     information for this type.  FIXME: there must be a better way to
     avoid divergence between compilations with and without debug
     information.  */
  {
    struct array_descr_info info;
    gfc_get_array_descr_info (fat_type, &info);
    gfc_get_array_descr_info (build_pointer_type (fat_type), &info);
  }

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

static tree gfc_nonrestricted_type (tree t);
/* Given two record or union type nodes TO and FROM, ensure
   that all fields in FROM have a corresponding field in TO,
   their type being nonrestrict variants.  This accepts a TO
   node that already has a prefix of the fields in FROM.  */
static void
mirror_fields (tree to, tree from)
{
  tree fto, ffrom;
  tree *chain;

  /* Forward to the end of TOs fields.  */
  fto = TYPE_FIELDS (to);
  ffrom = TYPE_FIELDS (from);
  chain = &TYPE_FIELDS (to);
  while (fto)
    {
      gcc_assert (ffrom && DECL_NAME (fto) == DECL_NAME (ffrom));
      chain = &DECL_CHAIN (fto);
      fto = DECL_CHAIN (fto);
      ffrom = DECL_CHAIN (ffrom);
    }

  /* Now add all fields remaining in FROM (starting with ffrom).  */
  for (; ffrom; ffrom = DECL_CHAIN (ffrom))
    {
      tree newfield = copy_node (ffrom);
      DECL_CONTEXT (newfield) = to;
      /* The store to DECL_CHAIN might seem redundant with the
	 stores to *chain, but not clearing it here would mean
	 leaving a chain into the old fields.  If ever
	 our called functions would look at them confusion
	 will arise.  */
      DECL_CHAIN (newfield) = NULL_TREE;
      *chain = newfield;
      chain = &DECL_CHAIN (newfield);

      if (TREE_CODE (ffrom) == FIELD_DECL)
	{
	  tree elemtype = gfc_nonrestricted_type (TREE_TYPE (ffrom));
	  TREE_TYPE (newfield) = elemtype;
	}
    }
  *chain = NULL_TREE;
}

/* Given a type T, returns a different type of the same structure,
   except that all types it refers to (recursively) are always
   non-restrict qualified types.  */
static tree
gfc_nonrestricted_type (tree t)
{
  tree ret = t;

  /* If the type isn't laid out yet, don't copy it.  If something
     needs it for real it should wait until the type got finished.  */
  if (!TYPE_SIZE (t))
    return t;

  if (!TYPE_LANG_SPECIFIC (t))
    TYPE_LANG_SPECIFIC (t)
      = ggc_alloc_cleared_lang_type (sizeof (struct lang_type));
  /* If we're dealing with this very node already further up
     the call chain (recursion via pointers and struct members)
     we haven't yet determined if we really need a new type node.
     Assume we don't, return T itself.  */
  if (TYPE_LANG_SPECIFIC (t)->nonrestricted_type == error_mark_node)
    return t;

  /* If we have calculated this all already, just return it.  */
  if (TYPE_LANG_SPECIFIC (t)->nonrestricted_type)
    return TYPE_LANG_SPECIFIC (t)->nonrestricted_type;

  /* Mark this type.  */
  TYPE_LANG_SPECIFIC (t)->nonrestricted_type = error_mark_node;

  switch (TREE_CODE (t))
    {
      default:
	break;

      case POINTER_TYPE:
      case REFERENCE_TYPE:
	{
	  tree totype = gfc_nonrestricted_type (TREE_TYPE (t));
	  if (totype == TREE_TYPE (t))
	    ret = t;
	  else if (TREE_CODE (t) == POINTER_TYPE)
	    ret = build_pointer_type (totype);
	  else
	    ret = build_reference_type (totype);
	  ret = build_qualified_type (ret,
				      TYPE_QUALS (t) & ~TYPE_QUAL_RESTRICT);
	}
	break;

      case ARRAY_TYPE:
	{
	  tree elemtype = gfc_nonrestricted_type (TREE_TYPE (t));
	  if (elemtype == TREE_TYPE (t))
	    ret = t;
	  else
	    {
	      ret = build_variant_type_copy (t);
	      TREE_TYPE (ret) = elemtype;
	      if (TYPE_LANG_SPECIFIC (t)
		  && GFC_TYPE_ARRAY_DATAPTR_TYPE (t))
		{
		  tree dataptr_type = GFC_TYPE_ARRAY_DATAPTR_TYPE (t);
		  dataptr_type = gfc_nonrestricted_type (dataptr_type);
		  if (dataptr_type != GFC_TYPE_ARRAY_DATAPTR_TYPE (t))
		    {
		      TYPE_LANG_SPECIFIC (ret)
			= ggc_alloc_cleared_lang_type (sizeof (struct
							       lang_type));
		      *TYPE_LANG_SPECIFIC (ret) = *TYPE_LANG_SPECIFIC (t);
		      GFC_TYPE_ARRAY_DATAPTR_TYPE (ret) = dataptr_type;
		    }
		}
	    }
	}
	break;

      case RECORD_TYPE:
      case UNION_TYPE:
      case QUAL_UNION_TYPE:
	{
	  tree field;
	  /* First determine if we need a new type at all.
	     Careful, the two calls to gfc_nonrestricted_type per field
	     might return different values.  That happens exactly when
	     one of the fields reaches back to this very record type
	     (via pointers).  The first calls will assume that we don't
	     need to copy T (see the error_mark_node marking).  If there
	     are any reasons for copying T apart from having to copy T,
	     we'll indeed copy it, and the second calls to
	     gfc_nonrestricted_type will use that new node if they
	     reach back to T.  */
	  for (field = TYPE_FIELDS (t); field; field = DECL_CHAIN (field))
	    if (TREE_CODE (field) == FIELD_DECL)
	      {
		tree elemtype = gfc_nonrestricted_type (TREE_TYPE (field));
		if (elemtype != TREE_TYPE (field))
		  break;
	      }
	  if (!field)
	    break;
	  ret = build_variant_type_copy (t);
	  TYPE_FIELDS (ret) = NULL_TREE;

	  /* Here we make sure that as soon as we know we have to copy
	     T, that also fields reaching back to us will use the new
	     copy.  It's okay if that copy still contains the old fields,
	     we won't look at them.  */
	  TYPE_LANG_SPECIFIC (t)->nonrestricted_type = ret;
	  mirror_fields (ret, t);
	}
        break;
    }

  TYPE_LANG_SPECIFIC (t)->nonrestricted_type = ret;
  return ret;
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
  bool restricted;

  /* Procedure Pointers inside COMMON blocks.  */
  if (sym->attr.proc_pointer && sym->attr.in_common)
    {
      /* Unset proc_pointer as gfc_get_function_type calls gfc_sym_type.  */
      sym->attr.proc_pointer = 0;
      type = build_pointer_type (gfc_get_function_type (sym));
      sym->attr.proc_pointer = 1;
      return type;
    }

  if (sym->attr.flavor == FL_PROCEDURE && !sym->attr.function)
    return void_type_node;

  /* In the case of a function the fake result variable may have a
     type different from the function type, so don't return early in
     that case.  */
  if (sym->backend_decl && !sym->attr.function)
    return TREE_TYPE (sym->backend_decl);

  if (sym->ts.type == BT_CHARACTER
      && ((sym->attr.function && sym->attr.is_bind_c)
	  || (sym->attr.result
	      && sym->ns->proc_name
	      && sym->ns->proc_name->attr.is_bind_c)))
    type = gfc_character1_type_node;
  else
    type = gfc_typenode_for_spec (&sym->ts);

  if (sym->attr.dummy && !sym->attr.function && !sym->attr.value)
    byref = 1;
  else
    byref = 0;

  restricted = !sym->attr.target && !sym->attr.pointer
               && !sym->attr.proc_pointer && !sym->attr.cray_pointee;
  if (!restricted)
    type = gfc_nonrestricted_type (type);

  if (sym->attr.dimension || sym->attr.codimension)
    {
      if (gfc_is_nodesc_array (sym))
        {
	  /* If this is a character argument of unknown length, just use the
	     base type.  */
	  if (sym->ts.type != BT_CHARACTER
	      || !(sym->attr.dummy || sym->attr.function)
	      || sym->ts.u.cl->backend_decl)
	    {
	      type = gfc_get_nodesc_array_type (type, sym->as,
						byref ? PACKED_FULL
						      : PACKED_STATIC,
						restricted);
	      byref = 0;
	    }

	  if (sym->attr.cray_pointee)
	    GFC_POINTER_TYPE_P (type) = 1;
        }
      else
	{
	  enum gfc_array_kind akind = GFC_ARRAY_UNKNOWN;
	  if (sym->attr.pointer)
	    akind = sym->attr.contiguous ? GFC_ARRAY_POINTER_CONT
					 : GFC_ARRAY_POINTER;
	  else if (sym->attr.allocatable)
	    akind = GFC_ARRAY_ALLOCATABLE;
	  type = gfc_build_array_type (type, sym->as, akind, restricted,
				       sym->attr.contiguous);
	}
    }
  else
    {
      if (sym->attr.allocatable || sym->attr.pointer
	  || gfc_is_associate_pointer (sym))
	type = gfc_build_pointer_type (sym, type);
      if (sym->attr.pointer || sym->attr.cray_pointee)
	GFC_POINTER_TYPE_P (type) = 1;
    }

  /* We currently pass all parameters by reference.
     See f95_get_function_decl.  For dummy function parameters return the
     function type.  */
  if (byref)
    {
      /* We must use pointer types for potentially absent variables.  The
	 optimizers assume a reference type argument is never NULL.  */
      if (sym->attr.optional
	  || (sym->ns->proc_name && sym->ns->proc_name->attr.entry_master))
	type = build_pointer_type (type);
      else
	{
	  type = build_reference_type (type);
	  if (restricted)
	    type = build_qualified_type (type, TYPE_QUAL_RESTRICT);
	}
    }

  return (type);
}

/* Layout and output debug info for a record type.  */

void
gfc_finish_type (tree type)
{
  tree decl;

  decl = build_decl (input_location,
		     TYPE_DECL, NULL_TREE, type);
  TYPE_STUB_DECL (type) = decl;
  layout_type (type);
  rest_of_type_compilation (type, 1);
  rest_of_decl_compilation (decl, 1, 0);
}

/* Add a field of given NAME and TYPE to the context of a UNION_TYPE
   or RECORD_TYPE pointed to by CONTEXT.  The new field is chained
   to the end of the field list pointed to by *CHAIN.

   Returns a pointer to the new field.  */

static tree
gfc_add_field_to_struct_1 (tree context, tree name, tree type, tree **chain)
{
  tree decl = build_decl (input_location, FIELD_DECL, name, type);

  DECL_CONTEXT (decl) = context;
  DECL_CHAIN (decl) = NULL_TREE;
  if (TYPE_FIELDS (context) == NULL_TREE)
    TYPE_FIELDS (context) = decl;
  if (chain != NULL)
    {
      if (*chain != NULL)
	**chain = decl;
      *chain = &DECL_CHAIN (decl);
    }

  return decl;
}

/* Like `gfc_add_field_to_struct_1', but adds alignment
   information.  */

tree
gfc_add_field_to_struct (tree context, tree name, tree type, tree **chain)
{
  tree decl = gfc_add_field_to_struct_1 (context, name, type, chain);

  DECL_INITIAL (decl) = 0;
  DECL_ALIGN (decl) = 0;
  DECL_USER_ALIGN (decl) = 0;

  return decl;
}


/* Copy the backend_decl and component backend_decls if
   the two derived type symbols are "equal", as described
   in 4.4.2 and resolved by gfc_compare_derived_types.  */

int
gfc_copy_dt_decls_ifequal (gfc_symbol *from, gfc_symbol *to,
			   bool from_gsym)
{
  gfc_component *to_cm;
  gfc_component *from_cm;

  if (from == to)
    return 1;

  if (from->backend_decl == NULL
	|| !gfc_compare_derived_types (from, to))
    return 0;

  to->backend_decl = from->backend_decl;

  to_cm = to->components;
  from_cm = from->components;

  /* Copy the component declarations.  If a component is itself
     a derived type, we need a copy of its component declarations.
     This is done by recursing into gfc_get_derived_type and
     ensures that the component's component declarations have
     been built.  If it is a character, we need the character
     length, as well.  */
  for (; to_cm; to_cm = to_cm->next, from_cm = from_cm->next)
    {
      to_cm->backend_decl = from_cm->backend_decl;
      if (from_cm->ts.type == BT_DERIVED
	  && (!from_cm->attr.pointer || from_gsym))
	gfc_get_derived_type (to_cm->ts.u.derived);
      else if (from_cm->ts.type == BT_CLASS
	       && (!CLASS_DATA (from_cm)->attr.class_pointer || from_gsym))
	gfc_get_derived_type (to_cm->ts.u.derived);
      else if (from_cm->ts.type == BT_CHARACTER)
	to_cm->ts.u.cl->backend_decl = from_cm->ts.u.cl->backend_decl;
    }

  return 1;
}


/* Build a tree node for a procedure pointer component.  */

tree
gfc_get_ppc_type (gfc_component* c)
{
  tree t;

  /* Explicit interface.  */
  if (c->attr.if_source != IFSRC_UNKNOWN && c->ts.interface)
    return build_pointer_type (gfc_get_function_type (c->ts.interface));

  /* Implicit interface (only return value may be known).  */
  if (c->attr.function && !c->attr.dimension && c->ts.type != BT_CHARACTER)
    t = gfc_typenode_for_spec (&c->ts);
  else
    t = void_type_node;

  return build_pointer_type (build_function_type_list (t, NULL_TREE));
}


/* Build a tree node for a derived type.  If there are equal
   derived types, with different local names, these are built
   at the same time.  If an equal derived type has been built
   in a parent namespace, this is used.  */

tree
gfc_get_derived_type (gfc_symbol * derived)
{
  tree typenode = NULL, field = NULL, field_type = NULL;
  tree canonical = NULL_TREE;
  tree *chain = NULL;
  bool got_canonical = false;
  bool unlimited_entity = false;
  gfc_component *c;
  gfc_dt_list *dt;
  gfc_namespace *ns;

  if (derived->attr.unlimited_polymorphic)
    return ptr_type_node;

  if (derived && derived->attr.flavor == FL_PROCEDURE
      && derived->attr.generic)
    derived = gfc_find_dt_in_generic (derived);

  /* See if it's one of the iso_c_binding derived types.  */
  if (derived->attr.is_iso_c == 1 || derived->ts.f90_type == BT_VOID)
    {
      if (derived->backend_decl)
	return derived->backend_decl;

      if (derived->intmod_sym_id == ISOCBINDING_PTR)
	derived->backend_decl = ptr_type_node;
      else
	derived->backend_decl = pfunc_type_node;

      derived->ts.kind = gfc_index_integer_kind;
      derived->ts.type = BT_INTEGER;
      /* Set the f90_type to BT_VOID as a way to recognize something of type
         BT_INTEGER that needs to fit a void * for the purpose of the
         iso_c_binding derived types.  */
      derived->ts.f90_type = BT_VOID;

      return derived->backend_decl;
    }

  /* If use associated, use the module type for this one.  */
  if (derived->backend_decl == NULL
      && derived->attr.use_assoc
      && derived->module
      && gfc_get_module_backend_decl (derived))
    goto copy_derived_types;

  /* The derived types from an earlier namespace can be used as the
     canonical type.  */
  if (derived->backend_decl == NULL && !derived->attr.use_assoc
      && gfc_global_ns_list)
    {
      for (ns = gfc_global_ns_list;
	   ns->translated && !got_canonical;
	   ns = ns->sibling)
	{
	  dt = ns->derived_types;
	  for (; dt && !canonical; dt = dt->next)
	    {
	      gfc_copy_dt_decls_ifequal (dt->derived, derived, true);
	      if (derived->backend_decl)
		got_canonical = true;
	    }
	}
    }

  /* Store up the canonical type to be added to this one.  */
  if (got_canonical)
    {
      if (TYPE_CANONICAL (derived->backend_decl))
	canonical = TYPE_CANONICAL (derived->backend_decl);
      else
	canonical = derived->backend_decl;

      derived->backend_decl = NULL_TREE;
    }

  /* derived->backend_decl != 0 means we saw it before, but its
     components' backend_decl may have not been built.  */
  if (derived->backend_decl)
    {
      /* Its components' backend_decl have been built or we are
	 seeing recursion through the formal arglist of a procedure
	 pointer component.  */
      if (TYPE_FIELDS (derived->backend_decl)
	    || derived->attr.proc_pointer_comp)
        return derived->backend_decl;
      else
        typenode = derived->backend_decl;
    }
  else
    {
      /* We see this derived type first time, so build the type node.  */
      typenode = make_node (RECORD_TYPE);
      TYPE_NAME (typenode) = get_identifier (derived->name);
      TYPE_PACKED (typenode) = gfc_option.flag_pack_derived;
      derived->backend_decl = typenode;
    }

  if (derived->components
	&& derived->components->ts.type == BT_DERIVED
	&& strcmp (derived->components->name, "_data") == 0
	&& derived->components->ts.u.derived->attr.unlimited_polymorphic)
    unlimited_entity = true;

  /* Go through the derived type components, building them as
     necessary. The reason for doing this now is that it is
     possible to recurse back to this derived type through a
     pointer component (PR24092). If this happens, the fields
     will be built and so we can return the type.  */
  for (c = derived->components; c; c = c->next)
    {
      if (c->ts.type != BT_DERIVED && c->ts.type != BT_CLASS)
	continue;

      if ((!c->attr.pointer && !c->attr.proc_pointer)
	  || c->ts.u.derived->backend_decl == NULL)
	c->ts.u.derived->backend_decl = gfc_get_derived_type (c->ts.u.derived);

      if (c->ts.u.derived->attr.is_iso_c)
        {
          /* Need to copy the modified ts from the derived type.  The
             typespec was modified because C_PTR/C_FUNPTR are translated
             into (void *) from derived types.  */
          c->ts.type = c->ts.u.derived->ts.type;
          c->ts.kind = c->ts.u.derived->ts.kind;
          c->ts.f90_type = c->ts.u.derived->ts.f90_type;
	  if (c->initializer)
	    {
	      c->initializer->ts.type = c->ts.type;
	      c->initializer->ts.kind = c->ts.kind;
	      c->initializer->ts.f90_type = c->ts.f90_type;
	      c->initializer->expr_type = EXPR_NULL;
	    }
        }
    }

  if (TYPE_FIELDS (derived->backend_decl))
    return derived->backend_decl;

  /* Build the type member list. Install the newly created RECORD_TYPE
     node as DECL_CONTEXT of each FIELD_DECL.  */
  for (c = derived->components; c; c = c->next)
    {
      if (c->attr.proc_pointer)
	field_type = gfc_get_ppc_type (c);
      else if (c->ts.type == BT_DERIVED || c->ts.type == BT_CLASS)
        field_type = c->ts.u.derived->backend_decl;
      else
	{
	  if (c->ts.type == BT_CHARACTER && !c->ts.deferred)
	    {
	      /* Evaluate the string length.  */
	      gfc_conv_const_charlen (c->ts.u.cl);
	      gcc_assert (c->ts.u.cl->backend_decl);
	    }
	  else if (c->ts.type == BT_CHARACTER)
	    c->ts.u.cl->backend_decl
			= build_int_cst (gfc_charlen_type_node, 0);

	  field_type = gfc_typenode_for_spec (&c->ts);
	}

      /* This returns an array descriptor type.  Initialization may be
         required.  */
      if ((c->attr.dimension || c->attr.codimension) && !c->attr.proc_pointer )
	{
	  if (c->attr.pointer || c->attr.allocatable)
	    {
	      enum gfc_array_kind akind;
	      if (c->attr.pointer)
		akind = c->attr.contiguous ? GFC_ARRAY_POINTER_CONT
					   : GFC_ARRAY_POINTER;
	      else
		akind = GFC_ARRAY_ALLOCATABLE;
	      /* Pointers to arrays aren't actually pointer types.  The
	         descriptors are separate, but the data is common.  */
	      field_type = gfc_build_array_type (field_type, c->as, akind,
						 !c->attr.target
						 && !c->attr.pointer,
						 c->attr.contiguous);
	    }
	  else
	    field_type = gfc_get_nodesc_array_type (field_type, c->as,
						    PACKED_STATIC,
						    !c->attr.target);
	}
      else if ((c->attr.pointer || c->attr.allocatable)
	       && !c->attr.proc_pointer
	       && !(unlimited_entity && c == derived->components))
	field_type = build_pointer_type (field_type);

      if (c->attr.pointer)
	field_type = gfc_nonrestricted_type (field_type);

      /* vtype fields can point to different types to the base type.  */
      if (c->ts.type == BT_DERIVED
	    && c->ts.u.derived && c->ts.u.derived->attr.vtype)
	  field_type = build_pointer_type_for_mode (TREE_TYPE (field_type),
						    ptr_mode, true);

      /* Ensure that the CLASS language specific flag is set.  */
      if (c->ts.type == BT_CLASS)
	{
	  if (POINTER_TYPE_P (field_type))
	    GFC_CLASS_TYPE_P (TREE_TYPE (field_type)) = 1;
	  else
	    GFC_CLASS_TYPE_P (field_type) = 1;
	}

      field = gfc_add_field_to_struct (typenode,
				       get_identifier (c->name),
				       field_type, &chain);
      if (c->loc.lb)
	gfc_set_decl_location (field, &c->loc);
      else if (derived->declared_at.lb)
	gfc_set_decl_location (field, &derived->declared_at);

      DECL_PACKED (field) |= TYPE_PACKED (typenode);

      gcc_assert (field);
      if (!c->backend_decl)
	c->backend_decl = field;
    }

  /* Now lay out the derived type, including the fields.  */
  if (canonical)
    TYPE_CANONICAL (typenode) = canonical;

  gfc_finish_type (typenode);
  gfc_set_decl_location (TYPE_STUB_DECL (typenode), &derived->declared_at);
  if (derived->module && derived->ns->proc_name
      && derived->ns->proc_name->attr.flavor == FL_MODULE)
    {
      if (derived->ns->proc_name->backend_decl
	  && TREE_CODE (derived->ns->proc_name->backend_decl)
	     == NAMESPACE_DECL)
	{
	  TYPE_CONTEXT (typenode) = derived->ns->proc_name->backend_decl;
	  DECL_CONTEXT (TYPE_STUB_DECL (typenode))
	    = derived->ns->proc_name->backend_decl;
	}
    }

  derived->backend_decl = typenode;

copy_derived_types:

  for (dt = gfc_derived_types; dt; dt = dt->next)
    gfc_copy_dt_decls_ifequal (derived, dt->derived, false);

  return derived->backend_decl;
}


int
gfc_return_by_reference (gfc_symbol * sym)
{
  if (!sym->attr.function)
    return 0;

  if (sym->attr.dimension)
    return 1;

  if (sym->ts.type == BT_CHARACTER
      && !sym->attr.is_bind_c
      && (!sym->attr.result
	  || !sym->ns->proc_name
	  || !sym->ns->proc_name->attr.is_bind_c))
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
  tree *chain = NULL;
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_entry_list *el, *el2;

  gcc_assert (ns->proc_name->attr.mixed_entry_master);
  gcc_assert (memcmp (ns->proc_name->name, "master.", 7) == 0);

  snprintf (name, GFC_MAX_SYMBOL_LEN, "munion.%s", ns->proc_name->name + 7);

  /* Build the type node.  */
  type = make_node (UNION_TYPE);

  TYPE_NAME (type) = get_identifier (name);

  for (el = ns->entries; el; el = el->next)
    {
      /* Search for duplicates.  */
      for (el2 = ns->entries; el2 != el; el2 = el2->next)
	if (el2->sym->result == el->sym->result)
	  break;

      if (el == el2)
	gfc_add_field_to_struct_1 (type,
				   get_identifier (el->sym->result->name),
				   gfc_sym_type (el->sym->result), &chain);
    }

  /* Finish off the type.  */
  gfc_finish_type (type);
  TYPE_DECL_SUPPRESS_DEBUG (TYPE_STUB_DECL (type)) = 1;
  return type;
}

/* Create a "fn spec" based on the formal arguments;
   cf. create_function_arglist.  */

static tree
create_fn_spec (gfc_symbol *sym, tree fntype)
{
  char spec[150];
  size_t spec_len;
  gfc_formal_arglist *f;
  tree tmp;

  memset (&spec, 0, sizeof (spec));
  spec[0] = '.';
  spec_len = 1;

  if (sym->attr.entry_master)
    spec[spec_len++] = 'R';
  if (gfc_return_by_reference (sym))
    {
      gfc_symbol *result = sym->result ? sym->result : sym;

      if (result->attr.pointer || sym->attr.proc_pointer)
	spec[spec_len++] = '.';
      else
	spec[spec_len++] = 'w';
      if (sym->ts.type == BT_CHARACTER)
	spec[spec_len++] = 'R';
    }

  for (f = gfc_sym_get_dummy_args (sym); f; f = f->next)
    if (spec_len < sizeof (spec))
      {
	if (!f->sym || f->sym->attr.pointer || f->sym->attr.target
	    || f->sym->attr.external || f->sym->attr.cray_pointer
	    || (f->sym->ts.type == BT_DERIVED
		&& (f->sym->ts.u.derived->attr.proc_pointer_comp
		    || f->sym->ts.u.derived->attr.pointer_comp))
	    || (f->sym->ts.type == BT_CLASS
		&& (CLASS_DATA (f->sym)->ts.u.derived->attr.proc_pointer_comp
		    || CLASS_DATA (f->sym)->ts.u.derived->attr.pointer_comp)))
	  spec[spec_len++] = '.';
	else if (f->sym->attr.intent == INTENT_IN)
	  spec[spec_len++] = 'r';
	else if (f->sym)
	  spec[spec_len++] = 'w';
      }

  tmp = build_tree_list (NULL_TREE, build_string (spec_len, spec));
  tmp = tree_cons (get_identifier ("fn spec"), tmp, TYPE_ATTRIBUTES (fntype));
  return build_type_attribute_variant (fntype, tmp);
}


tree
gfc_get_function_type (gfc_symbol * sym)
{
  tree type;
  vec<tree, va_gc> *typelist = NULL;
  gfc_formal_arglist *f;
  gfc_symbol *arg;
  int alternate_return = 0;
  bool is_varargs = true;

  /* Make sure this symbol is a function, a subroutine or the main
     program.  */
  gcc_assert (sym->attr.flavor == FL_PROCEDURE
	      || sym->attr.flavor == FL_PROGRAM);

  /* To avoid recursing infinitely on recursive types, we use error_mark_node
     so that they can be detected here and handled further down.  */
  if (sym->backend_decl == NULL)
    sym->backend_decl = error_mark_node;
  else if (sym->backend_decl == error_mark_node)
    goto arg_type_list_done;
  else if (sym->attr.proc_pointer)
    return TREE_TYPE (TREE_TYPE (sym->backend_decl));
  else
    return TREE_TYPE (sym->backend_decl);

  if (sym->attr.entry_master)
    /* Additional parameter for selecting an entry point.  */
    vec_safe_push (typelist, gfc_array_index_type);

  if (sym->result)
    arg = sym->result;
  else
    arg = sym;

  if (arg->ts.type == BT_CHARACTER)
    gfc_conv_const_charlen (arg->ts.u.cl);

  /* Some functions we use an extra parameter for the return value.  */
  if (gfc_return_by_reference (sym))
    {
      type = gfc_sym_type (arg);
      if (arg->ts.type == BT_COMPLEX
	  || arg->attr.dimension
	  || arg->ts.type == BT_CHARACTER)
	type = build_reference_type (type);

      vec_safe_push (typelist, type);
      if (arg->ts.type == BT_CHARACTER)
	{
	  if (!arg->ts.deferred)
	    /* Transfer by value.  */
	    vec_safe_push (typelist, gfc_charlen_type_node);
	  else
	    /* Deferred character lengths are transferred by reference
	       so that the value can be returned.  */
	    vec_safe_push (typelist, build_pointer_type(gfc_charlen_type_node));
	}
    }

  /* Build the argument types for the function.  */
  for (f = gfc_sym_get_dummy_args (sym); f; f = f->next)
    {
      arg = f->sym;
      if (arg)
	{
	  /* Evaluate constant character lengths here so that they can be
	     included in the type.  */
	  if (arg->ts.type == BT_CHARACTER)
	    gfc_conv_const_charlen (arg->ts.u.cl);

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
	     used without an explicit interface, and cannot be passed as
	     actual parameters for a dummy procedure.  */

	  vec_safe_push (typelist, type);
	}
      else
        {
          if (sym->attr.subroutine)
            alternate_return = 1;
        }
    }

  /* Add hidden string length parameters.  */
  for (f = gfc_sym_get_dummy_args (sym); f; f = f->next)
    {
      arg = f->sym;
      if (arg && arg->ts.type == BT_CHARACTER && !sym->attr.is_bind_c)
	{
	  if (!arg->ts.deferred)
	    /* Transfer by value.  */
	    type = gfc_charlen_type_node;
	  else
	    /* Deferred character lengths are transferred by reference
	       so that the value can be returned.  */
	    type = build_pointer_type (gfc_charlen_type_node);

	  vec_safe_push (typelist, type);
	}
    }

  if (!vec_safe_is_empty (typelist)
      || sym->attr.is_main_program
      || sym->attr.if_source != IFSRC_UNKNOWN)
    is_varargs = false;

  if (sym->backend_decl == error_mark_node)
    sym->backend_decl = NULL_TREE;

arg_type_list_done:

  if (alternate_return)
    type = integer_type_node;
  else if (!sym->attr.function || gfc_return_by_reference (sym))
    type = void_type_node;
  else if (sym->attr.mixed_entry_master)
    type = gfc_get_mixed_entry_union (sym->ns);
  else if (gfc_option.flag_f2c
	   && sym->ts.type == BT_REAL
	   && sym->ts.kind == gfc_default_real_kind
	   && !sym->attr.always_explicit)
    {
      /* Special case: f2c calling conventions require that (scalar)
	 default REAL functions return the C type double instead.  f2c
	 compatibility is only an issue with functions that don't
	 require an explicit interface, as only these could be
	 implemented in Fortran 77.  */
      sym->ts.kind = gfc_default_double_kind;
      type = gfc_typenode_for_spec (&sym->ts);
      sym->ts.kind = gfc_default_real_kind;
    }
  else if (sym->result && sym->result->attr.proc_pointer)
    /* Procedure pointer return values.  */
    {
      if (sym->result->attr.result && strcmp (sym->name,"ppr@") != 0)
	{
	  /* Unset proc_pointer as gfc_get_function_type
	     is called recursively.  */
	  sym->result->attr.proc_pointer = 0;
	  type = build_pointer_type (gfc_get_function_type (sym->result));
	  sym->result->attr.proc_pointer = 1;
	}
      else
       type = gfc_sym_type (sym->result);
    }
  else
    type = gfc_sym_type (sym);

  if (is_varargs)
    type = build_varargs_function_type_vec (type, typelist);
  else
    type = build_function_type_vec (type, typelist);
  type = create_fn_spec (sym, type);

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

      /* Handle TImode as a special case because it is used by some backends
         (e.g. ARM) even though it is not available for normal use.  */
#if HOST_BITS_PER_WIDE_INT >= 64
      if (bits == TYPE_PRECISION (intTI_type_node))
	return intTI_type_node;
#endif

      if (bits <= TYPE_PRECISION (intQI_type_node))
	return intQI_type_node;
      if (bits <= TYPE_PRECISION (intHI_type_node))
	return intHI_type_node;
      if (bits <= TYPE_PRECISION (intSI_type_node))
	return intSI_type_node;
      if (bits <= TYPE_PRECISION (intDI_type_node))
	return intDI_type_node;
      if (bits <= TYPE_PRECISION (intTI_type_node))
	return intTI_type_node;
    }
  else
    {
      if (bits <= TYPE_PRECISION (unsigned_intQI_type_node))
        return unsigned_intQI_type_node;
      if (bits <= TYPE_PRECISION (unsigned_intHI_type_node))
	return unsigned_intHI_type_node;
      if (bits <= TYPE_PRECISION (unsigned_intSI_type_node))
	return unsigned_intSI_type_node;
      if (bits <= TYPE_PRECISION (unsigned_intDI_type_node))
	return unsigned_intDI_type_node;
      if (bits <= TYPE_PRECISION (unsigned_intTI_type_node))
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
    {
      tree type = gfc_type_for_size (GET_MODE_PRECISION (mode), unsignedp);
      return type != NULL_TREE && mode == TYPE_MODE (type) ? type : NULL_TREE;
    }
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

/* Return TRUE if TYPE is a type with a hidden descriptor, fill in INFO
   in that case.  */

bool
gfc_get_array_descr_info (const_tree type, struct array_descr_info *info)
{
  int rank, dim;
  bool indirect = false;
  tree etype, ptype, field, t, base_decl;
  tree data_off, dim_off, dim_size, elem_size;
  tree lower_suboff, upper_suboff, stride_suboff;

  if (! GFC_DESCRIPTOR_TYPE_P (type))
    {
      if (! POINTER_TYPE_P (type))
	return false;
      type = TREE_TYPE (type);
      if (! GFC_DESCRIPTOR_TYPE_P (type))
	return false;
      indirect = true;
    }

  rank = GFC_TYPE_ARRAY_RANK (type);
  if (rank >= (int) (sizeof (info->dimen) / sizeof (info->dimen[0])))
    return false;

  etype = GFC_TYPE_ARRAY_DATAPTR_TYPE (type);
  gcc_assert (POINTER_TYPE_P (etype));
  etype = TREE_TYPE (etype);

  /* If the type is not a scalar coarray.  */
  if (TREE_CODE (etype) == ARRAY_TYPE)
    etype = TREE_TYPE (etype);

  /* Can't handle variable sized elements yet.  */
  if (int_size_in_bytes (etype) <= 0)
    return false;
  /* Nor non-constant lower bounds in assumed shape arrays.  */
  if (GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_ASSUMED_SHAPE
      || GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_ASSUMED_SHAPE_CONT)
    {
      for (dim = 0; dim < rank; dim++)
	if (GFC_TYPE_ARRAY_LBOUND (type, dim) == NULL_TREE
	    || TREE_CODE (GFC_TYPE_ARRAY_LBOUND (type, dim)) != INTEGER_CST)
	  return false;
    }

  memset (info, '\0', sizeof (*info));
  info->ndimensions = rank;
  info->element_type = etype;
  ptype = build_pointer_type (gfc_array_index_type);
  base_decl = GFC_TYPE_ARRAY_BASE_DECL (type, indirect);
  if (!base_decl)
    {
      base_decl = build_decl (input_location, VAR_DECL, NULL_TREE,
			      indirect ? build_pointer_type (ptype) : ptype);
      GFC_TYPE_ARRAY_BASE_DECL (type, indirect) = base_decl;
    }
  info->base_decl = base_decl;
  if (indirect)
    base_decl = build1 (INDIRECT_REF, ptype, base_decl);

  if (GFC_TYPE_ARRAY_SPAN (type))
    elem_size = GFC_TYPE_ARRAY_SPAN (type);
  else
    elem_size = fold_convert (gfc_array_index_type, TYPE_SIZE_UNIT (etype));
  field = TYPE_FIELDS (TYPE_MAIN_VARIANT (type));
  data_off = byte_position (field);
  field = DECL_CHAIN (field);
  field = DECL_CHAIN (field);
  field = DECL_CHAIN (field);
  dim_off = byte_position (field);
  dim_size = TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (field)));
  field = TYPE_FIELDS (TREE_TYPE (TREE_TYPE (field)));
  stride_suboff = byte_position (field);
  field = DECL_CHAIN (field);
  lower_suboff = byte_position (field);
  field = DECL_CHAIN (field);
  upper_suboff = byte_position (field);

  t = base_decl;
  if (!integer_zerop (data_off))
    t = fold_build_pointer_plus (t, data_off);
  t = build1 (NOP_EXPR, build_pointer_type (ptr_type_node), t);
  info->data_location = build1 (INDIRECT_REF, ptr_type_node, t);
  if (GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_ALLOCATABLE)
    info->allocated = build2 (NE_EXPR, boolean_type_node,
			      info->data_location, null_pointer_node);
  else if (GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_POINTER
	   || GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_POINTER_CONT)
    info->associated = build2 (NE_EXPR, boolean_type_node,
			       info->data_location, null_pointer_node);

  for (dim = 0; dim < rank; dim++)
    {
      t = fold_build_pointer_plus (base_decl,
				   size_binop (PLUS_EXPR,
					       dim_off, lower_suboff));
      t = build1 (INDIRECT_REF, gfc_array_index_type, t);
      info->dimen[dim].lower_bound = t;
      t = fold_build_pointer_plus (base_decl,
				   size_binop (PLUS_EXPR,
					       dim_off, upper_suboff));
      t = build1 (INDIRECT_REF, gfc_array_index_type, t);
      info->dimen[dim].upper_bound = t;
      if (GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_ASSUMED_SHAPE
	  || GFC_TYPE_ARRAY_AKIND (type) == GFC_ARRAY_ASSUMED_SHAPE_CONT)
	{
	  /* Assumed shape arrays have known lower bounds.  */
	  info->dimen[dim].upper_bound
	    = build2 (MINUS_EXPR, gfc_array_index_type,
		      info->dimen[dim].upper_bound,
		      info->dimen[dim].lower_bound);
	  info->dimen[dim].lower_bound
	    = fold_convert (gfc_array_index_type,
			    GFC_TYPE_ARRAY_LBOUND (type, dim));
	  info->dimen[dim].upper_bound
	    = build2 (PLUS_EXPR, gfc_array_index_type,
		      info->dimen[dim].lower_bound,
		      info->dimen[dim].upper_bound);
	}
      t = fold_build_pointer_plus (base_decl,
				   size_binop (PLUS_EXPR,
					       dim_off, stride_suboff));
      t = build1 (INDIRECT_REF, gfc_array_index_type, t);
      t = build2 (MULT_EXPR, gfc_array_index_type, t, elem_size);
      info->dimen[dim].stride = t;
      dim_off = size_binop (PLUS_EXPR, dim_off, dim_size);
    }

  return true;
}

#include "gt-fortran-trans-types.h"
