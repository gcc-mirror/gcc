/* PR tree-optimization/101254 */
/* { dg-do run } */
/* { dg-options "-O2 -fwrapv" } */

int
foo (long long imin, long long imax)
{
  if (imin > imax)
    return 0;
  else if (imax - imin < 0 || (imax - imin) + 1 < 0)
    return 0;
  return 1;
}

int
main ()
{
  long long imax = __LONG_LONG_MAX__;
  long long imin = -imax - 1; 
  if (!foo (-10, 10))
    __builtin_abort ();
  if (foo (-10, imax))
    __builtin_abort ();
  if (foo (imin, imax))
    __builtin_abort ();
  return 0;
}
