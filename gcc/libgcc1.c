/* Subroutines needed by GCC output code on some machines.  */
/* Compile this file with the Unix C compiler!  */
/* Copyright (C) 1987, 1988, 1992 Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file with other programs, and to distribute
those programs without any restriction coming from the use of this
file.  (The General Public License restrictions do apply in other
respects; for example, they cover modification of the file, and
distribution when not linked into another program.)

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#include "config.h"

/* Don't use `fancy_abort' here even if config.h says to use it.  */
#ifdef abort
#undef abort
#endif

/* On some machines, cc is really GCC.  For these machines, we can't
   expect these functions to be properly compiled unless GCC open codes
   the operation (which is precisely when the function won't be used).
   So allow tm.h to specify ways of accomplishing the operations
   by defining the macros perform_*.

   On a machine where cc is some other compiler, there is usually no
   reason to define perform_*.  The other compiler normally has other ways
   of implementing all of these operations.

   In some cases a certain machine may come with GCC installed as cc
   or may have some other compiler.  Then it may make sense for tm.h
   to define perform_* only if __GNUC__ is defined.  */

#ifndef perform_mulsi3
#define perform_mulsi3(a, b) return a * b
#endif

#ifndef perform_divsi3
#define perform_divsi3(a, b) return a / b
#endif

#ifndef perform_udivsi3
#define perform_udivsi3(a, b) return a / b
#endif

#ifndef perform_modsi3
#define perform_modsi3(a, b) return a % b
#endif

#ifndef perform_umodsi3
#define perform_umodsi3(a, b) return a % b
#endif

#ifndef perform_lshrsi3
#define perform_lshrsi3(a, b) return a >> b
#endif

#ifndef perform_lshlsi3
#define perform_lshlsi3(a, b) return a << b
#endif

#ifndef perform_ashrsi3
#define perform_ashrsi3(a, b) return a >> b
#endif

#ifndef perform_ashlsi3
#define perform_ashlsi3(a, b) return a << b
#endif

#ifndef perform_adddf3
#define perform_adddf3(a, b) return a + b
#endif

#ifndef perform_subdf3
#define perform_subdf3(a, b) return a - b
#endif

#ifndef perform_muldf3
#define perform_muldf3(a, b) return a * b
#endif

#ifndef perform_divdf3
#define perform_divdf3(a, b) return a / b
#endif

#ifndef perform_addsf3
#define perform_addsf3(a, b) return INTIFY (a + b)
#endif

#ifndef perform_subsf3
#define perform_subsf3(a, b) return INTIFY (a - b)
#endif

#ifndef perform_mulsf3
#define perform_mulsf3(a, b) return INTIFY (a * b)
#endif

#ifndef perform_divsf3
#define perform_divsf3(a, b) return INTIFY (a / b)
#endif

#ifndef perform_negdf2
#define perform_negdf2(a) return -a
#endif

#ifndef perform_negsf2
#define perform_negsf2(a) return INTIFY (-a)
#endif

#ifndef perform_fixdfsi
#define perform_fixdfsi(a) return (nongcc_SI_type) a;
#endif

#ifndef perform_fixsfsi
#define perform_fixsfsi(a) return (nongcc_SI_type) a
#endif

#ifndef perform_floatsidf
#define perform_floatsidf(a) return (double) a
#endif

#ifndef perform_floatsisf
#define perform_floatsisf(a)  return INTIFY ((float) a)
#endif

#ifndef perform_extendsfdf2
#define perform_extendsfdf2(a)  return a
#endif

#ifndef perform_truncdfsf2
#define perform_truncdfsf2(a)  return INTIFY (a)
#endif

/* Note that eqdf2 returns a value for "true" that is == 0,
   nedf2 returns a value for "true" that is != 0,
   gtdf2 returns a value for "true" that is > 0,
   and so on.  */

#ifndef perform_eqdf2
#define perform_eqdf2(a, b) return !(a == b)
#endif

#ifndef perform_nedf2
#define perform_nedf2(a, b) return a != b
#endif

#ifndef perform_gtdf2
#define perform_gtdf2(a, b) return a > b
#endif

#ifndef perform_gedf2
#define perform_gedf2(a, b) return (a >= b) - 1
#endif

#ifndef perform_ltdf2
#define perform_ltdf2(a, b) return -(a < b)
#endif

#ifndef perform_ledf2
#define perform_ledf2(a, b) return 1 - (a <= b)
#endif

#ifndef perform_eqsf2
#define perform_eqsf2(a, b) return !(a == b)
#endif

#ifndef perform_nesf2
#define perform_nesf2(a, b) return a != b
#endif

#ifndef perform_gtsf2
#define perform_gtsf2(a, b) return a > b
#endif

#ifndef perform_gesf2
#define perform_gesf2(a, b) return (a >= b) - 1
#endif

#ifndef perform_ltsf2
#define perform_ltsf2(a, b) return -(a < b)
#endif

#ifndef perform_lesf2
#define perform_lesf2(a, b) return 1 - (a <= b);
#endif

/* Define the C data type to use for an SImode value.  */

#ifndef nongcc_SI_type
#define nongcc_SI_type long int
#endif

/* Define the type to be used for returning an SF mode value
   and the method for turning a float into that type.
   These definitions work for machines where an SF value is
   returned in the same register as an int.  */

#ifndef FLOAT_VALUE_TYPE  
#define FLOAT_VALUE_TYPE int
#endif

#ifndef INTIFY
#define INTIFY(FLOATVAL)  (intify.f = (FLOATVAL), intify.i)
#endif

#ifndef FLOATIFY
#define FLOATIFY(INTVAL)  ((INTVAL).f)
#endif

#ifndef FLOAT_ARG_TYPE
#define FLOAT_ARG_TYPE union flt_or_int
#endif

union flt_or_value { FLOAT_VALUE_TYPE i; float f; };

union flt_or_int { int i; float f; };


#ifdef L_mulsi3
nongcc_SI_type
__mulsi3 (a, b)
     nongcc_SI_type a, b;
{
  perform_mulsi3 (a, b);
}
#endif

#ifdef L_udivsi3
nongcc_SI_type
__udivsi3 (a, b)
     unsigned nongcc_SI_type a, b;
{
  perform_udivsi3 (a, b);
}
#endif

#ifdef L_divsi3
nongcc_SI_type
__divsi3 (a, b)
     nongcc_SI_type a, b;
{
  perform_divsi3 (a, b);
}
#endif

#ifdef L_umodsi3
nongcc_SI_type
__umodsi3 (a, b)
     unsigned nongcc_SI_type a, b;
{
  perform_umodsi3 (a, b);
}
#endif

#ifdef L_modsi3
nongcc_SI_type
__modsi3 (a, b)
     nongcc_SI_type a, b;
{
  perform_modsi3 (a, b);
}
#endif

#ifdef L_lshrsi3
nongcc_SI_type
__lshrsi3 (a, b)
     unsigned nongcc_SI_type a, b;
{
  perform_lshrsi3 (a, b);
}
#endif

#ifdef L_lshlsi3
nongcc_SI_type
__lshlsi3 (a, b)
     unsigned nongcc_SI_type a, b;
{
  perform_lshlsi3 (a, b);
}
#endif

#ifdef L_ashrsi3
nongcc_SI_type
__ashrsi3 (a, b)
     nongcc_SI_type a, b;
{
  perform_ashrsi3 (a, b);
}
#endif

#ifdef L_ashlsi3
nongcc_SI_type
__ashlsi3 (a, b)
     nongcc_SI_type a, b;
{
  perform_ashlsi3 (a, b);
}
#endif

#ifdef L_divdf3
double
__divdf3 (a, b)
     double a, b;
{
  perform_divdf3 (a, b);
}
#endif

#ifdef L_muldf3
double
__muldf3 (a, b)
     double a, b;
{
  perform_muldf3 (a, b);
}
#endif

#ifdef L_negdf2
double
__negdf2 (a)
     double a;
{
  perform_negdf2 (a);
}
#endif

#ifdef L_adddf3
double
__adddf3 (a, b)
     double a, b;
{
  perform_adddf3 (a, b);
}
#endif

#ifdef L_subdf3
double
__subdf3 (a, b)
     double a, b;
{
  perform_subdf3 (a, b);
}
#endif

/* Note that eqdf2 returns a value for "true" that is == 0,
   nedf2 returns a value for "true" that is != 0,
   gtdf2 returns a value for "true" that is > 0,
   and so on.  */

#ifdef L_eqdf2
nongcc_SI_type
__eqdf2 (a, b)
     double a, b;
{
  /* Value == 0 iff a == b.  */
  perform_eqdf2 (a, b);
}
#endif

#ifdef L_nedf2
nongcc_SI_type
__nedf2 (a, b)
     double a, b;
{
  /* Value != 0 iff a != b.  */
  perform_nedf2 (a, b);
}
#endif

#ifdef L_gtdf2
nongcc_SI_type
__gtdf2 (a, b)
     double a, b;
{
  /* Value > 0 iff a > b.  */
  perform_gtdf2 (a, b);
}
#endif

#ifdef L_gedf2
nongcc_SI_type
__gedf2 (a, b)
     double a, b;
{
  /* Value >= 0 iff a >= b.  */
  perform_gedf2 (a, b);
}
#endif

#ifdef L_ltdf2
nongcc_SI_type
__ltdf2 (a, b)
     double a, b;
{
  /* Value < 0 iff a < b.  */
  perform_ltdf2 (a, b);
}
#endif

#ifdef L_ledf2
nongcc_SI_type
__ledf2 (a, b)
     double a, b;
{
  /* Value <= 0 iff a <= b.  */
  perform_ledf2 (a, b);
}
#endif

#ifdef L_fixdfsi
nongcc_SI_type
__fixdfsi (a)
     double a;
{
  perform_fixdfsi (a);
}
#endif

#ifdef L_fixsfsi
nongcc_SI_type
__fixsfsi (a)
     FLOAT_ARG_TYPE a;
{
  union flt_or_value intify;
  perform_fixsfsi (FLOATIFY (a));
}
#endif

#ifdef L_floatsidf
double
__floatsidf (a)
     nongcc_SI_type a;
{
  perform_floatsidf (a);
}
#endif

#ifdef L_floatsisf
FLOAT_VALUE_TYPE
__floatsisf (a)
     nongcc_SI_type a;
{
  union flt_or_value intify;
  perform_floatsisf (a);
}
#endif

#ifdef L_addsf3
FLOAT_VALUE_TYPE
__addsf3 (a, b)
     FLOAT_ARG_TYPE a, b;
{
  union flt_or_value intify;
  perform_addsf3 (FLOATIFY (a), FLOATIFY (b));
}
#endif

#ifdef L_negsf2
FLOAT_VALUE_TYPE
__negsf2 (a)
     FLOAT_ARG_TYPE a;
{
  union flt_or_value intify;
  perform_negsf2 (FLOATIFY (a));
}
#endif

#ifdef L_subsf3
FLOAT_VALUE_TYPE
__subsf3 (a, b)
     FLOAT_ARG_TYPE a, b;
{
  union flt_or_value intify;
  perform_subsf3 (FLOATIFY (a), FLOATIFY (b));
}
#endif

#ifdef L_eqsf2
nongcc_SI_type
__eqsf2 (a, b)
     FLOAT_ARG_TYPE a, b;
{
  union flt_or_int intify;
  /* Value == 0 iff a == b.  */
  perform_eqsf2 (FLOATIFY (a), FLOATIFY (b));
}
#endif

#ifdef L_nesf2
nongcc_SI_type
__nesf2 (a, b)
     FLOAT_ARG_TYPE a, b;
{
  union flt_or_int intify;
  /* Value != 0 iff a != b.  */
  perform_nesf2 (FLOATIFY (a), FLOATIFY (b));
}
#endif

#ifdef L_gtsf2
nongcc_SI_type
__gtsf2 (a, b)
     FLOAT_ARG_TYPE a, b;
{
  union flt_or_int intify;
  /* Value > 0 iff a > b.  */
  perform_gtsf2 (FLOATIFY (a), FLOATIFY (b));
}
#endif

#ifdef L_gesf2
nongcc_SI_type
__gesf2 (a, b)
     FLOAT_ARG_TYPE a, b;
{
  union flt_or_int intify;
  /* Value >= 0 iff a >= b.  */
  perform_gesf2 (FLOATIFY (a), FLOATIFY (b));
}
#endif

#ifdef L_ltsf2
nongcc_SI_type
__ltsf2 (a, b)
     FLOAT_ARG_TYPE a, b;
{
  union flt_or_int intify;
  /* Value < 0 iff a < b.  */
  perform_ltsf2 (FLOATIFY (a), FLOATIFY (b));
}
#endif

#ifdef L_lesf2
nongcc_SI_type
__lesf2 (a, b)
     FLOAT_ARG_TYPE a, b;
{
  union flt_or_int intify;
  /* Value <= 0 iff a <= b.  */
  perform_lesf2 (FLOATIFY (a), FLOATIFY (b));
}
#endif

#ifdef L_mulsf3
FLOAT_VALUE_TYPE
__mulsf3 (a, b)
     FLOAT_ARG_TYPE a, b;
{
  union flt_or_value intify;
  perform_mulsf3 (FLOATIFY (a), FLOATIFY (b));
}
#endif

#ifdef L_divsf3
FLOAT_VALUE_TYPE
__divsf3 (a, b)
     FLOAT_ARG_TYPE a, b;
{
  union flt_or_value intify;
  perform_divsf3 (FLOATIFY (a), FLOATIFY (b));
}
#endif

#ifdef L_truncdfsf2
FLOAT_VALUE_TYPE
__truncdfsf2 (a)
     double a;
{
  union flt_or_value intify;
  perform_truncdfsf2 (a);
}
#endif

#ifdef L_extendsfdf2
double
__extendsfdf2 (a)
     FLOAT_ARG_TYPE a;
{
  union flt_or_value intify;
  perform_extendsfdf2 (FLOATIFY (a));
}
#endif
