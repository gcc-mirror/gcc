/* { dg-do compile } */
/* { dg-options "-O2" }  */

extern void overflow_handler ();

unsigned long long overflow_sub (unsigned long long x, unsigned long long y)
{
  unsigned long long r;

  int ovr = __builtin_usubll_overflow (x, y, &r);
  if (ovr)
    overflow_handler ();

  return r;
}

/* { dg-final { scan-assembler "subs" } } */

