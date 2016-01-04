/* Mapping from optabs to underlying library functions
   Copyright (C) 1987-2016 Free Software Foundation, Inc.

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
#include "target.h"
#include "insn-codes.h"
#include "optabs-libfuncs.h"
#include "libfuncs.h"
#include "optabs-query.h"
#include "tree.h"
#include "stringpool.h"
#include "varasm.h"
#include "stor-layout.h"
#include "rtl.h"

struct target_libfuncs default_target_libfuncs;
#if SWITCHABLE_TARGET
struct target_libfuncs *this_target_libfuncs = &default_target_libfuncs;
#endif

#define libfunc_hash \
  (this_target_libfuncs->x_libfunc_hash)

/* Prefixes for the current version of decimal floating point (BID vs. DPD) */
#if ENABLE_DECIMAL_BID_FORMAT
#define DECIMAL_PREFIX "bid_"
#else
#define DECIMAL_PREFIX "dpd_"
#endif

/* Used for libfunc_hash.  */

hashval_t
libfunc_hasher::hash (libfunc_entry *e)
{
  return ((e->mode1 + e->mode2 * NUM_MACHINE_MODES) ^ e->op);
}

/* Used for libfunc_hash.  */

bool
libfunc_hasher::equal (libfunc_entry *e1, libfunc_entry *e2)
{
  return e1->op == e2->op && e1->mode1 == e2->mode1 && e1->mode2 == e2->mode2;
}

/* Return libfunc corresponding operation defined by OPTAB converting
   from MODE2 to MODE1.  Trigger lazy initialization if needed, return NULL
   if no libfunc is available.  */
rtx
convert_optab_libfunc (convert_optab optab, machine_mode mode1,
		       machine_mode mode2)
{
  struct libfunc_entry e;
  struct libfunc_entry **slot;

  /* ??? This ought to be an assert, but not all of the places
     that we expand optabs know about the optabs that got moved
     to being direct.  */
  if (!(optab >= FIRST_CONV_OPTAB && optab <= LAST_CONVLIB_OPTAB))
    return NULL_RTX;

  e.op = optab;
  e.mode1 = mode1;
  e.mode2 = mode2;
  slot = libfunc_hash->find_slot (&e, NO_INSERT);
  if (!slot)
    {
      const struct convert_optab_libcall_d *d
	= &convlib_def[optab - FIRST_CONV_OPTAB];

      if (d->libcall_gen == NULL)
	return NULL;

      d->libcall_gen (optab, d->libcall_basename, mode1, mode2);
      slot = libfunc_hash->find_slot (&e, NO_INSERT);
      if (!slot)
	return NULL;
    }
  return (*slot)->libfunc;
}

/* Return libfunc corresponding operation defined by OPTAB in MODE.
   Trigger lazy initialization if needed, return NULL if no libfunc is
   available.  */
rtx
optab_libfunc (optab optab, machine_mode mode)
{
  struct libfunc_entry e;
  struct libfunc_entry **slot;

  /* ??? This ought to be an assert, but not all of the places
     that we expand optabs know about the optabs that got moved
     to being direct.  */
  if (!(optab >= FIRST_NORM_OPTAB && optab <= LAST_NORMLIB_OPTAB))
    return NULL_RTX;

  e.op = optab;
  e.mode1 = mode;
  e.mode2 = VOIDmode;
  slot = libfunc_hash->find_slot (&e, NO_INSERT);
  if (!slot)
    {
      const struct optab_libcall_d *d
	= &normlib_def[optab - FIRST_NORM_OPTAB];

      if (d->libcall_gen == NULL)
	return NULL;

      d->libcall_gen (optab, d->libcall_basename, d->libcall_suffix, mode);
      slot = libfunc_hash->find_slot (&e, NO_INSERT);
      if (!slot)
	return NULL;
    }
  return (*slot)->libfunc;
}

/* Initialize the libfunc fields of an entire group of entries in some
   optab.  Each entry is set equal to a string consisting of a leading
   pair of underscores followed by a generic operation name followed by
   a mode name (downshifted to lowercase) followed by a single character
   representing the number of operands for the given operation (which is
   usually one of the characters '2', '3', or '4').

   OPTABLE is the table in which libfunc fields are to be initialized.
   OPNAME is the generic (string) name of the operation.
   SUFFIX is the character which specifies the number of operands for
     the given generic operation.
   MODE is the mode to generate for.  */

static void
gen_libfunc (optab optable, const char *opname, int suffix,
	     machine_mode mode)
{
  unsigned opname_len = strlen (opname);
  const char *mname = GET_MODE_NAME (mode);
  unsigned mname_len = strlen (mname);
  int prefix_len = targetm.libfunc_gnu_prefix ? 6 : 2;
  int len = prefix_len + opname_len + mname_len + 1 + 1;
  char *libfunc_name = XALLOCAVEC (char, len);
  char *p;
  const char *q;

  p = libfunc_name;
  *p++ = '_';
  *p++ = '_';
  if (targetm.libfunc_gnu_prefix)
    {
      *p++ = 'g';
      *p++ = 'n';
      *p++ = 'u';
      *p++ = '_';
    }
  for (q = opname; *q;)
    *p++ = *q++;
  for (q = mname; *q; q++)
    *p++ = TOLOWER (*q);
  *p++ = suffix;
  *p = '\0';

  set_optab_libfunc (optable, mode,
		     ggc_alloc_string (libfunc_name, p - libfunc_name));
}

/* Like gen_libfunc, but verify that integer operation is involved.  */

void
gen_int_libfunc (optab optable, const char *opname, char suffix,
		 machine_mode mode)
{
  int maxsize = 2 * BITS_PER_WORD;
  int minsize = BITS_PER_WORD;

  if (GET_MODE_CLASS (mode) != MODE_INT)
    return;
  if (maxsize < LONG_LONG_TYPE_SIZE)
    maxsize = LONG_LONG_TYPE_SIZE;
  if (minsize > INT_TYPE_SIZE
      && (trapv_binoptab_p (optable)
	  || trapv_unoptab_p (optable)))
    minsize = INT_TYPE_SIZE;
  if (GET_MODE_BITSIZE (mode) < minsize
      || GET_MODE_BITSIZE (mode) > maxsize)
    return;
  gen_libfunc (optable, opname, suffix, mode);
}

/* Like gen_libfunc, but verify that FP and set decimal prefix if needed.  */

void
gen_fp_libfunc (optab optable, const char *opname, char suffix,
		machine_mode mode)
{
  char *dec_opname;

  if (GET_MODE_CLASS (mode) == MODE_FLOAT)
    gen_libfunc (optable, opname, suffix, mode);
  if (DECIMAL_FLOAT_MODE_P (mode))
    {
      dec_opname = XALLOCAVEC (char, sizeof (DECIMAL_PREFIX) + strlen (opname));
      /* For BID support, change the name to have either a bid_ or dpd_ prefix
	 depending on the low level floating format used.  */
      memcpy (dec_opname, DECIMAL_PREFIX, sizeof (DECIMAL_PREFIX) - 1);
      strcpy (dec_opname + sizeof (DECIMAL_PREFIX) - 1, opname);
      gen_libfunc (optable, dec_opname, suffix, mode);
    }
}

/* Like gen_libfunc, but verify that fixed-point operation is involved.  */

void
gen_fixed_libfunc (optab optable, const char *opname, char suffix,
		   machine_mode mode)
{
  if (!ALL_FIXED_POINT_MODE_P (mode))
    return;
  gen_libfunc (optable, opname, suffix, mode);
}

/* Like gen_libfunc, but verify that signed fixed-point operation is
   involved.  */

void
gen_signed_fixed_libfunc (optab optable, const char *opname, char suffix,
			  machine_mode mode)
{
  if (!SIGNED_FIXED_POINT_MODE_P (mode))
    return;
  gen_libfunc (optable, opname, suffix, mode);
}

/* Like gen_libfunc, but verify that unsigned fixed-point operation is
   involved.  */

void
gen_unsigned_fixed_libfunc (optab optable, const char *opname, char suffix,
			    machine_mode mode)
{
  if (!UNSIGNED_FIXED_POINT_MODE_P (mode))
    return;
  gen_libfunc (optable, opname, suffix, mode);
}

/* Like gen_libfunc, but verify that FP or INT operation is involved.  */

void
gen_int_fp_libfunc (optab optable, const char *name, char suffix,
		    machine_mode mode)
{
  if (DECIMAL_FLOAT_MODE_P (mode) || GET_MODE_CLASS (mode) == MODE_FLOAT)
    gen_fp_libfunc (optable, name, suffix, mode);
  if (INTEGRAL_MODE_P (mode))
    gen_int_libfunc (optable, name, suffix, mode);
}

/* Like gen_libfunc, but verify that FP or INT operation is involved
   and add 'v' suffix for integer operation.  */

void
gen_intv_fp_libfunc (optab optable, const char *name, char suffix,
		     machine_mode mode)
{
  if (DECIMAL_FLOAT_MODE_P (mode) || GET_MODE_CLASS (mode) == MODE_FLOAT)
    gen_fp_libfunc (optable, name, suffix, mode);
  if (GET_MODE_CLASS (mode) == MODE_INT)
    {
      int len = strlen (name);
      char *v_name = XALLOCAVEC (char, len + 2);
      strcpy (v_name, name);
      v_name[len] = 'v';
      v_name[len + 1] = 0;
      gen_int_libfunc (optable, v_name, suffix, mode);
    }
}

/* Like gen_libfunc, but verify that FP or INT or FIXED operation is
   involved.  */

void
gen_int_fp_fixed_libfunc (optab optable, const char *name, char suffix,
			  machine_mode mode)
{
  if (DECIMAL_FLOAT_MODE_P (mode) || GET_MODE_CLASS (mode) == MODE_FLOAT)
    gen_fp_libfunc (optable, name, suffix, mode);
  if (INTEGRAL_MODE_P (mode))
    gen_int_libfunc (optable, name, suffix, mode);
  if (ALL_FIXED_POINT_MODE_P (mode))
    gen_fixed_libfunc (optable, name, suffix, mode);
}

/* Like gen_libfunc, but verify that FP or INT or signed FIXED operation is
   involved.  */

void
gen_int_fp_signed_fixed_libfunc (optab optable, const char *name, char suffix,
				 machine_mode mode)
{
  if (DECIMAL_FLOAT_MODE_P (mode) || GET_MODE_CLASS (mode) == MODE_FLOAT)
    gen_fp_libfunc (optable, name, suffix, mode);
  if (INTEGRAL_MODE_P (mode))
    gen_int_libfunc (optable, name, suffix, mode);
  if (SIGNED_FIXED_POINT_MODE_P (mode))
    gen_signed_fixed_libfunc (optable, name, suffix, mode);
}

/* Like gen_libfunc, but verify that INT or FIXED operation is
   involved.  */

void
gen_int_fixed_libfunc (optab optable, const char *name, char suffix,
		       machine_mode mode)
{
  if (INTEGRAL_MODE_P (mode))
    gen_int_libfunc (optable, name, suffix, mode);
  if (ALL_FIXED_POINT_MODE_P (mode))
    gen_fixed_libfunc (optable, name, suffix, mode);
}

/* Like gen_libfunc, but verify that INT or signed FIXED operation is
   involved.  */

void
gen_int_signed_fixed_libfunc (optab optable, const char *name, char suffix,
			      machine_mode mode)
{
  if (INTEGRAL_MODE_P (mode))
    gen_int_libfunc (optable, name, suffix, mode);
  if (SIGNED_FIXED_POINT_MODE_P (mode))
    gen_signed_fixed_libfunc (optable, name, suffix, mode);
}

/* Like gen_libfunc, but verify that INT or unsigned FIXED operation is
   involved.  */

void
gen_int_unsigned_fixed_libfunc (optab optable, const char *name, char suffix,
				machine_mode mode)
{
  if (INTEGRAL_MODE_P (mode))
    gen_int_libfunc (optable, name, suffix, mode);
  if (UNSIGNED_FIXED_POINT_MODE_P (mode))
    gen_unsigned_fixed_libfunc (optable, name, suffix, mode);
}

/* Initialize the libfunc fields of an entire group of entries of an
   inter-mode-class conversion optab.  The string formation rules are
   similar to the ones for init_libfuncs, above, but instead of having
   a mode name and an operand count these functions have two mode names
   and no operand count.  */

void
gen_interclass_conv_libfunc (convert_optab tab,
			     const char *opname,
			     machine_mode tmode,
			     machine_mode fmode)
{
  size_t opname_len = strlen (opname);
  size_t mname_len = 0;

  const char *fname, *tname;
  const char *q;
  int prefix_len = targetm.libfunc_gnu_prefix ? 6 : 2;
  char *libfunc_name, *suffix;
  char *nondec_name, *dec_name, *nondec_suffix, *dec_suffix;
  char *p;

  /* If this is a decimal conversion, add the current BID vs. DPD prefix that
     depends on which underlying decimal floating point format is used.  */
  const size_t dec_len = sizeof (DECIMAL_PREFIX) - 1;

  mname_len = strlen (GET_MODE_NAME (tmode)) + strlen (GET_MODE_NAME (fmode));

  nondec_name = XALLOCAVEC (char, prefix_len + opname_len + mname_len + 1 + 1);
  nondec_name[0] = '_';
  nondec_name[1] = '_';
  if (targetm.libfunc_gnu_prefix)
    {
      nondec_name[2] = 'g';
      nondec_name[3] = 'n';
      nondec_name[4] = 'u';
      nondec_name[5] = '_';
    }

  memcpy (&nondec_name[prefix_len], opname, opname_len);
  nondec_suffix = nondec_name + opname_len + prefix_len;

  dec_name = XALLOCAVEC (char, 2 + dec_len + opname_len + mname_len + 1 + 1);
  dec_name[0] = '_';
  dec_name[1] = '_';
  memcpy (&dec_name[2], DECIMAL_PREFIX, dec_len);
  memcpy (&dec_name[2+dec_len], opname, opname_len);
  dec_suffix = dec_name + dec_len + opname_len + 2;

  fname = GET_MODE_NAME (fmode);
  tname = GET_MODE_NAME (tmode);

  if (DECIMAL_FLOAT_MODE_P (fmode) || DECIMAL_FLOAT_MODE_P (tmode))
    {
      libfunc_name = dec_name;
      suffix = dec_suffix;
    }
  else
    {
      libfunc_name = nondec_name;
      suffix = nondec_suffix;
    }

  p = suffix;
  for (q = fname; *q; p++, q++)
    *p = TOLOWER (*q);
  for (q = tname; *q; p++, q++)
    *p = TOLOWER (*q);

  *p = '\0';

  set_conv_libfunc (tab, tmode, fmode,
		    ggc_alloc_string (libfunc_name, p - libfunc_name));
}

/* Same as gen_interclass_conv_libfunc but verify that we are producing
   int->fp conversion.  */

void
gen_int_to_fp_conv_libfunc (convert_optab tab,
			    const char *opname,
			    machine_mode tmode,
			    machine_mode fmode)
{
  if (GET_MODE_CLASS (fmode) != MODE_INT)
    return;
  if (GET_MODE_CLASS (tmode) != MODE_FLOAT && !DECIMAL_FLOAT_MODE_P (tmode))
    return;
  gen_interclass_conv_libfunc (tab, opname, tmode, fmode);
}

/* ufloat_optab is special by using floatun for FP and floatuns decimal fp
   naming scheme.  */

void
gen_ufloat_conv_libfunc (convert_optab tab,
			 const char *opname ATTRIBUTE_UNUSED,
			 machine_mode tmode,
			 machine_mode fmode)
{
  if (DECIMAL_FLOAT_MODE_P (tmode))
    gen_int_to_fp_conv_libfunc (tab, "floatuns", tmode, fmode);
  else
    gen_int_to_fp_conv_libfunc (tab, "floatun", tmode, fmode);
}

/* Same as gen_interclass_conv_libfunc but verify that we are producing
   fp->int conversion.  */

void
gen_int_to_fp_nondecimal_conv_libfunc (convert_optab tab,
				       const char *opname,
				       machine_mode tmode,
				       machine_mode fmode)
{
  if (GET_MODE_CLASS (fmode) != MODE_INT)
    return;
  if (GET_MODE_CLASS (tmode) != MODE_FLOAT)
    return;
  gen_interclass_conv_libfunc (tab, opname, tmode, fmode);
}

/* Same as gen_interclass_conv_libfunc but verify that we are producing
   fp->int conversion with no decimal floating point involved.  */

void
gen_fp_to_int_conv_libfunc (convert_optab tab,
			    const char *opname,
			    machine_mode tmode,
			    machine_mode fmode)
{
  if (GET_MODE_CLASS (fmode) != MODE_FLOAT && !DECIMAL_FLOAT_MODE_P (fmode))
    return;
  if (GET_MODE_CLASS (tmode) != MODE_INT)
    return;
  gen_interclass_conv_libfunc (tab, opname, tmode, fmode);
}

/* Initialize the libfunc fields of an of an intra-mode-class conversion optab.
   The string formation rules are
   similar to the ones for init_libfunc, above.  */

void
gen_intraclass_conv_libfunc (convert_optab tab, const char *opname,
			     machine_mode tmode, machine_mode fmode)
{
  size_t opname_len = strlen (opname);
  size_t mname_len = 0;

  const char *fname, *tname;
  const char *q;
  int prefix_len = targetm.libfunc_gnu_prefix ? 6 : 2;
  char *nondec_name, *dec_name, *nondec_suffix, *dec_suffix;
  char *libfunc_name, *suffix;
  char *p;

  /* If this is a decimal conversion, add the current BID vs. DPD prefix that
     depends on which underlying decimal floating point format is used.  */
  const size_t dec_len = sizeof (DECIMAL_PREFIX) - 1;

  mname_len = strlen (GET_MODE_NAME (tmode)) + strlen (GET_MODE_NAME (fmode));

  nondec_name = XALLOCAVEC (char, 2 + opname_len + mname_len + 1 + 1);
  nondec_name[0] = '_';
  nondec_name[1] = '_';
  if (targetm.libfunc_gnu_prefix)
    {
      nondec_name[2] = 'g';
      nondec_name[3] = 'n';
      nondec_name[4] = 'u';
      nondec_name[5] = '_';
    }
  memcpy (&nondec_name[prefix_len], opname, opname_len);
  nondec_suffix = nondec_name + opname_len + prefix_len;

  dec_name = XALLOCAVEC (char, 2 + dec_len + opname_len + mname_len + 1 + 1);
  dec_name[0] = '_';
  dec_name[1] = '_';
  memcpy (&dec_name[2], DECIMAL_PREFIX, dec_len);
  memcpy (&dec_name[2 + dec_len], opname, opname_len);
  dec_suffix = dec_name + dec_len + opname_len + 2;

  fname = GET_MODE_NAME (fmode);
  tname = GET_MODE_NAME (tmode);

  if (DECIMAL_FLOAT_MODE_P (fmode) || DECIMAL_FLOAT_MODE_P (tmode))
    {
      libfunc_name = dec_name;
      suffix = dec_suffix;
    }
  else
    {
      libfunc_name = nondec_name;
      suffix = nondec_suffix;
    }

  p = suffix;
  for (q = fname; *q; p++, q++)
    *p = TOLOWER (*q);
  for (q = tname; *q; p++, q++)
    *p = TOLOWER (*q);

  *p++ = '2';
  *p = '\0';

  set_conv_libfunc (tab, tmode, fmode,
		    ggc_alloc_string (libfunc_name, p - libfunc_name));
}

/* Pick proper libcall for trunc_optab.  We need to chose if we do
   truncation or extension and interclass or intraclass.  */

void
gen_trunc_conv_libfunc (convert_optab tab,
			const char *opname,
			machine_mode tmode,
			machine_mode fmode)
{
  if (GET_MODE_CLASS (tmode) != MODE_FLOAT && !DECIMAL_FLOAT_MODE_P (tmode))
    return;
  if (GET_MODE_CLASS (fmode) != MODE_FLOAT && !DECIMAL_FLOAT_MODE_P (fmode))
    return;
  if (tmode == fmode)
    return;

  if ((GET_MODE_CLASS (tmode) == MODE_FLOAT && DECIMAL_FLOAT_MODE_P (fmode))
      || (GET_MODE_CLASS (fmode) == MODE_FLOAT && DECIMAL_FLOAT_MODE_P (tmode)))
     gen_interclass_conv_libfunc (tab, opname, tmode, fmode);

  if (GET_MODE_PRECISION (fmode) <= GET_MODE_PRECISION (tmode))
    return;

  if ((GET_MODE_CLASS (tmode) == MODE_FLOAT
       && GET_MODE_CLASS (fmode) == MODE_FLOAT)
      || (DECIMAL_FLOAT_MODE_P (fmode) && DECIMAL_FLOAT_MODE_P (tmode)))
    gen_intraclass_conv_libfunc (tab, opname, tmode, fmode);
}

/* Pick proper libcall for extend_optab.  We need to chose if we do
   truncation or extension and interclass or intraclass.  */

void
gen_extend_conv_libfunc (convert_optab tab,
			 const char *opname ATTRIBUTE_UNUSED,
			 machine_mode tmode,
			 machine_mode fmode)
{
  if (GET_MODE_CLASS (tmode) != MODE_FLOAT && !DECIMAL_FLOAT_MODE_P (tmode))
    return;
  if (GET_MODE_CLASS (fmode) != MODE_FLOAT && !DECIMAL_FLOAT_MODE_P (fmode))
    return;
  if (tmode == fmode)
    return;

  if ((GET_MODE_CLASS (tmode) == MODE_FLOAT && DECIMAL_FLOAT_MODE_P (fmode))
      || (GET_MODE_CLASS (fmode) == MODE_FLOAT && DECIMAL_FLOAT_MODE_P (tmode)))
     gen_interclass_conv_libfunc (tab, opname, tmode, fmode);

  if (GET_MODE_PRECISION (fmode) > GET_MODE_PRECISION (tmode))
    return;

  if ((GET_MODE_CLASS (tmode) == MODE_FLOAT
       && GET_MODE_CLASS (fmode) == MODE_FLOAT)
      || (DECIMAL_FLOAT_MODE_P (fmode) && DECIMAL_FLOAT_MODE_P (tmode)))
    gen_intraclass_conv_libfunc (tab, opname, tmode, fmode);
}

/* Pick proper libcall for fract_optab.  We need to chose if we do
   interclass or intraclass.  */

void
gen_fract_conv_libfunc (convert_optab tab,
			const char *opname,
			machine_mode tmode,
			machine_mode fmode)
{
  if (tmode == fmode)
    return;
  if (!(ALL_FIXED_POINT_MODE_P (tmode) || ALL_FIXED_POINT_MODE_P (fmode)))
    return;

  if (GET_MODE_CLASS (tmode) == GET_MODE_CLASS (fmode))
    gen_intraclass_conv_libfunc (tab, opname, tmode, fmode);
  else
    gen_interclass_conv_libfunc (tab, opname, tmode, fmode);
}

/* Pick proper libcall for fractuns_optab.  */

void
gen_fractuns_conv_libfunc (convert_optab tab,
			   const char *opname,
			   machine_mode tmode,
			   machine_mode fmode)
{
  if (tmode == fmode)
    return;
  /* One mode must be a fixed-point mode, and the other must be an integer
     mode.  */
  if (!((ALL_FIXED_POINT_MODE_P (tmode) && GET_MODE_CLASS (fmode) == MODE_INT)
	|| (ALL_FIXED_POINT_MODE_P (fmode)
	    && GET_MODE_CLASS (tmode) == MODE_INT)))
    return;

  gen_interclass_conv_libfunc (tab, opname, tmode, fmode);
}

/* Pick proper libcall for satfract_optab.  We need to chose if we do
   interclass or intraclass.  */

void
gen_satfract_conv_libfunc (convert_optab tab,
			   const char *opname,
			   machine_mode tmode,
			   machine_mode fmode)
{
  if (tmode == fmode)
    return;
  /* TMODE must be a fixed-point mode.  */
  if (!ALL_FIXED_POINT_MODE_P (tmode))
    return;

  if (GET_MODE_CLASS (tmode) == GET_MODE_CLASS (fmode))
    gen_intraclass_conv_libfunc (tab, opname, tmode, fmode);
  else
    gen_interclass_conv_libfunc (tab, opname, tmode, fmode);
}

/* Pick proper libcall for satfractuns_optab.  */

void
gen_satfractuns_conv_libfunc (convert_optab tab,
			      const char *opname,
			      machine_mode tmode,
			      machine_mode fmode)
{
  if (tmode == fmode)
    return;
  /* TMODE must be a fixed-point mode, and FMODE must be an integer mode.  */
  if (!(ALL_FIXED_POINT_MODE_P (tmode) && GET_MODE_CLASS (fmode) == MODE_INT))
    return;

  gen_interclass_conv_libfunc (tab, opname, tmode, fmode);
}

/* Hashtable callbacks for libfunc_decls.  */

struct libfunc_decl_hasher : ggc_ptr_hash<tree_node>
{
  static hashval_t
  hash (tree entry)
  {
    return IDENTIFIER_HASH_VALUE (DECL_NAME (entry));
  }

  static bool
  equal (tree decl, tree name)
  {
    return DECL_NAME (decl) == name;
  }
};

/* A table of previously-created libfuncs, hashed by name.  */
static GTY (()) hash_table<libfunc_decl_hasher> *libfunc_decls;

/* Build a decl for a libfunc named NAME.  */

tree
build_libfunc_function (const char *name)
{
  tree decl = build_decl (UNKNOWN_LOCATION, FUNCTION_DECL,
			  get_identifier (name),
			  build_function_type (integer_type_node, NULL_TREE));
  /* ??? We don't have any type information except for this is
     a function.  Pretend this is "int foo ()".  */
  DECL_ARTIFICIAL (decl) = 1;
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  gcc_assert (DECL_ASSEMBLER_NAME (decl));

  /* Zap the nonsensical SYMBOL_REF_DECL for this.  What we're left with
     are the flags assigned by targetm.encode_section_info.  */
  SET_SYMBOL_REF_DECL (XEXP (DECL_RTL (decl), 0), NULL);

  return decl;
}

/* Return a libfunc for NAME, creating one if we don't already have one.
   The returned rtx is a SYMBOL_REF.  */

rtx
init_one_libfunc (const char *name)
{
  tree id, decl;
  hashval_t hash;

  if (libfunc_decls == NULL)
    libfunc_decls = hash_table<libfunc_decl_hasher>::create_ggc (37);

  /* See if we have already created a libfunc decl for this function.  */
  id = get_identifier (name);
  hash = IDENTIFIER_HASH_VALUE (id);
  tree *slot = libfunc_decls->find_slot_with_hash (id, hash, INSERT);
  decl = *slot;
  if (decl == NULL)
    {
      /* Create a new decl, so that it can be passed to
	 targetm.encode_section_info.  */
      decl = build_libfunc_function (name);
      *slot = decl;
    }
  return XEXP (DECL_RTL (decl), 0);
}

/* Adjust the assembler name of libfunc NAME to ASMSPEC.  */

rtx
set_user_assembler_libfunc (const char *name, const char *asmspec)
{
  tree id, decl;
  hashval_t hash;

  id = get_identifier (name);
  hash = IDENTIFIER_HASH_VALUE (id);
  tree *slot = libfunc_decls->find_slot_with_hash (id, hash, NO_INSERT);
  gcc_assert (slot);
  decl = (tree) *slot;
  set_user_assembler_name (decl, asmspec);
  return XEXP (DECL_RTL (decl), 0);
}

/* Call this to reset the function entry for one optab (OPTABLE) in mode
   MODE to NAME, which should be either 0 or a string constant.  */

void
set_optab_libfunc (optab op, machine_mode mode, const char *name)
{
  rtx val;
  struct libfunc_entry e;
  struct libfunc_entry **slot;

  e.op = op;
  e.mode1 = mode;
  e.mode2 = VOIDmode;

  if (name)
    val = init_one_libfunc (name);
  else
    val = 0;
  slot = libfunc_hash->find_slot (&e, INSERT);
  if (*slot == NULL)
    *slot = ggc_alloc<libfunc_entry> ();
  (*slot)->op = op;
  (*slot)->mode1 = mode;
  (*slot)->mode2 = VOIDmode;
  (*slot)->libfunc = val;
}

/* Call this to reset the function entry for one conversion optab
   (OPTABLE) from mode FMODE to mode TMODE to NAME, which should be
   either 0 or a string constant.  */

void
set_conv_libfunc (convert_optab optab, machine_mode tmode,
		  machine_mode fmode, const char *name)
{
  rtx val;
  struct libfunc_entry e;
  struct libfunc_entry **slot;

  e.op = optab;
  e.mode1 = tmode;
  e.mode2 = fmode;

  if (name)
    val = init_one_libfunc (name);
  else
    val = 0;
  slot = libfunc_hash->find_slot (&e, INSERT);
  if (*slot == NULL)
    *slot = ggc_alloc<libfunc_entry> ();
  (*slot)->op = optab;
  (*slot)->mode1 = tmode;
  (*slot)->mode2 = fmode;
  (*slot)->libfunc = val;
}

/* Call this to initialize the contents of the optabs
   appropriately for the current target machine.  */

void
init_optabs (void)
{
  if (libfunc_hash)
    libfunc_hash->empty ();
  else
    libfunc_hash = hash_table<libfunc_hasher>::create_ggc (10);

  /* Fill in the optabs with the insns we support.  */
  init_all_optabs (this_fn_optabs);

  /* The ffs function operates on `int'.  Fall back on it if we do not
     have a libgcc2 function for that width.  */
  if (INT_TYPE_SIZE < BITS_PER_WORD)
    set_optab_libfunc (ffs_optab, mode_for_size (INT_TYPE_SIZE, MODE_INT, 0),
		       "ffs");

  /* Explicitly initialize the bswap libfuncs since we need them to be
     valid for things other than word_mode.  */
  if (targetm.libfunc_gnu_prefix)
    {
      set_optab_libfunc (bswap_optab, SImode, "__gnu_bswapsi2");
      set_optab_libfunc (bswap_optab, DImode, "__gnu_bswapdi2");
    }
  else
    {
      set_optab_libfunc (bswap_optab, SImode, "__bswapsi2");
      set_optab_libfunc (bswap_optab, DImode, "__bswapdi2");
    }

  /* Use cabs for double complex abs, since systems generally have cabs.
     Don't define any libcall for float complex, so that cabs will be used.  */
  if (complex_double_type_node)
    set_optab_libfunc (abs_optab, TYPE_MODE (complex_double_type_node),
		       "cabs");

  abort_libfunc = init_one_libfunc ("abort");
  memcpy_libfunc = init_one_libfunc ("memcpy");
  memmove_libfunc = init_one_libfunc ("memmove");
  memcmp_libfunc = init_one_libfunc ("memcmp");
  memset_libfunc = init_one_libfunc ("memset");
  setbits_libfunc = init_one_libfunc ("__setbits");

#ifndef DONT_USE_BUILTIN_SETJMP
  setjmp_libfunc = init_one_libfunc ("__builtin_setjmp");
  longjmp_libfunc = init_one_libfunc ("__builtin_longjmp");
#else
  setjmp_libfunc = init_one_libfunc ("setjmp");
  longjmp_libfunc = init_one_libfunc ("longjmp");
#endif
  unwind_sjlj_register_libfunc = init_one_libfunc ("_Unwind_SjLj_Register");
  unwind_sjlj_unregister_libfunc
    = init_one_libfunc ("_Unwind_SjLj_Unregister");

  /* For function entry/exit instrumentation.  */
  profile_function_entry_libfunc
    = init_one_libfunc ("__cyg_profile_func_enter");
  profile_function_exit_libfunc
    = init_one_libfunc ("__cyg_profile_func_exit");

  gcov_flush_libfunc = init_one_libfunc ("__gcov_flush");

  /* Allow the target to add more libcalls or rename some, etc.  */
  targetm.init_libfuncs ();
}

/* A helper function for init_sync_libfuncs.  Using the basename BASE,
   install libfuncs into TAB for BASE_N for 1 <= N <= MAX.  */

static void
init_sync_libfuncs_1 (optab tab, const char *base, int max)
{
  machine_mode mode;
  char buf[64];
  size_t len = strlen (base);
  int i;

  gcc_assert (max <= 8);
  gcc_assert (len + 3 < sizeof (buf));

  memcpy (buf, base, len);
  buf[len] = '_';
  buf[len + 1] = '0';
  buf[len + 2] = '\0';

  mode = QImode;
  for (i = 1; i <= max; i *= 2)
    {
      buf[len + 1] = '0' + i;
      set_optab_libfunc (tab, mode, buf);
      mode = GET_MODE_2XWIDER_MODE (mode);
    }
}

void
init_sync_libfuncs (int max)
{
  if (!flag_sync_libcalls)
    return;

  init_sync_libfuncs_1 (sync_compare_and_swap_optab,
			"__sync_val_compare_and_swap", max);
  init_sync_libfuncs_1 (sync_lock_test_and_set_optab,
			"__sync_lock_test_and_set", max);

  init_sync_libfuncs_1 (sync_old_add_optab, "__sync_fetch_and_add", max);
  init_sync_libfuncs_1 (sync_old_sub_optab, "__sync_fetch_and_sub", max);
  init_sync_libfuncs_1 (sync_old_ior_optab, "__sync_fetch_and_or", max);
  init_sync_libfuncs_1 (sync_old_and_optab, "__sync_fetch_and_and", max);
  init_sync_libfuncs_1 (sync_old_xor_optab, "__sync_fetch_and_xor", max);
  init_sync_libfuncs_1 (sync_old_nand_optab, "__sync_fetch_and_nand", max);

  init_sync_libfuncs_1 (sync_new_add_optab, "__sync_add_and_fetch", max);
  init_sync_libfuncs_1 (sync_new_sub_optab, "__sync_sub_and_fetch", max);
  init_sync_libfuncs_1 (sync_new_ior_optab, "__sync_or_and_fetch", max);
  init_sync_libfuncs_1 (sync_new_and_optab, "__sync_and_and_fetch", max);
  init_sync_libfuncs_1 (sync_new_xor_optab, "__sync_xor_and_fetch", max);
  init_sync_libfuncs_1 (sync_new_nand_optab, "__sync_nand_and_fetch", max);
}

#include "gt-optabs-libfuncs.h"
