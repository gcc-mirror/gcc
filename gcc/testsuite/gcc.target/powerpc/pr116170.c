/* { dg-do compile } */
/* { dg-require-effective-target ppc_float128_sw } */
/* { dg-options "-mdejagnu-cpu=power8 -O2 -fstack-protector-strong -ffloat-store -std=gnu17" } */

/* Verify there is no ICE.  */

int a, d;
_Float128 b, c;
void
e ()
{
  int f = 0;
  if (a)
    if (b || c)
      f = 1;
  if (d)
    e (f ? 0 : b);
}
