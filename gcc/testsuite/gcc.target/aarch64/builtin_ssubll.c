/* { dg-do compile } */
/* { dg-options "-O2" }  */

extern void overflow_handler ();

long long overflow_sub (long long x, long long y)
{
  long long r;

  int ovr = __builtin_ssubll_overflow (x, y, &r);
  if (ovr)
    overflow_handler ();

  return r;
}

/* { dg-final { scan-assembler "subs" } } */

