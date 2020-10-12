/* { dg-do run } */
/* { dg-require-effective-target dfprt } */
/* { dg-options "-O2" } */

#include <stdio.h>
#include <stdlib.h>

/* Runnable test case for testing _Decimal128 to _Decimal32 rounding.
   The value below when rounded to _Decimal64 would result in the value
   1.2345675e+00, which if it were rounded to _Decimal32 would result in
   the value 1.234568e+00.  However, the correct value when rounding from
   _Decimal128  directly to _Decimal32 is 1.234567e+00.  */

_Decimal128 td = 1.23456749999999999999e+00dl;
_Decimal32 sd_expected = 1.234567e+00df;

_Decimal32 __attribute__((noinline))
td2sd (_Decimal128 td)
{
  return td;
}

int
main (void)
{
  _Decimal32 sd = td2sd (td);
  if (sd != sd_expected)
    {
      union {
	_Decimal32 sd;
	unsigned int i;
      } u;

      printf ("cast to _Decimal32 failed:\n");
      u.sd = sd;
      printf ("  actual   = 0x%x\n", u.i);
      u.sd = sd_expected;
      printf ("  expected = 0x%x\n", u.i);
      abort ();
    }

  return 0;
}
