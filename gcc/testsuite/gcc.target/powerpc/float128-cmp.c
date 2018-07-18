/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-options "-mvsx -O2" } */

#include <stddef.h>
#include <stdlib.h>

#ifndef TYPE
#define TYPE __float128
#define NAN __builtin_nanq ("")
#define SNAN __builtin_nansq ("")
#else
#define NAN __builtin_nan ("")
#define SNAN __builtin_nans ("")
#endif

extern void check (TYPE a,
		   TYPE b,
		   int eq,
		   int ne,
		   int lt,
		   int le,
		   int gt,
		   int ge,
		   int i_lt,
		   int i_le,
		   int i_gt,
		   int i_ge,
		   int i_lg,
		   int i_un) __attribute__((__noinline__));

void
check (TYPE a,
       TYPE b,
       int eq,
       int ne,
       int lt,
       int le,
       int gt,
       int ge,
       int i_lt,
       int i_le,
       int i_gt,
       int i_ge,
       int i_lg,
       int i_un)
{
  if (eq != (a == b))
    abort ();

  if (ne != (a != b))
    abort ();

  if (lt != (a < b))
    abort ();

  if (le != (a <= b))
    abort ();

  if (gt != (a > b))
    abort ();

  if (ge != (a >= b))
    abort ();

  if (i_lt != __builtin_isless (a, b))
    abort ();

  if (i_le != __builtin_islessequal (a, b))
    abort ();

  if (i_gt != __builtin_isgreater (a, b))
    abort ();

  if (i_ge != __builtin_isgreaterequal (a, b))
    abort ();

  if (i_lg != __builtin_islessgreater (a, b))
    abort ();

  if (i_un != __builtin_isunordered (a, b))
    abort ();
}

int main (void)
{
  TYPE one   = (TYPE) +1.0;
  TYPE two   = (TYPE) +2.0;
  TYPE pzero = (TYPE) +0.0;
  TYPE mzero = (TYPE) -0.0;
  TYPE nan   = (TYPE) NAN;
  TYPE snan  = (TYPE) SNAN;

  check (one,   two,   0, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 0);
  check (one,   one,   1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0);
  check (one,   pzero, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0);
  check (mzero, pzero, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0);
  check (nan,   one,   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1);
  check (one,   nan,   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1);
  check (nan,   nan,   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1);
  check (snan,  one,   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1);
  check (one,   snan,  0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1);
  check (snan,  nan,   0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1);
  check (nan,   snan,  0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1);
  return 0;
}
