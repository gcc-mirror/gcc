/* { dg-do compile } */
/* { dg-options "-O2" }  */

extern void overflow_handler ();

long overflow_sub (long x, long y)
{
  long r;

  int ovr = __builtin_ssubl_overflow (x, y, &r);
  if (ovr)
    overflow_handler ();

  return r;
}

/* { dg-final { scan-assembler "subs" } } */
