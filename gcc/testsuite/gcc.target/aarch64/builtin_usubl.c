/* { dg-do compile } */
/* { dg-options "-O2" }  */

extern void overflow_handler ();

unsigned long overflow_sub (unsigned long x, unsigned long y)
{
  unsigned long r;

  int ovr = __builtin_usubl_overflow (x, y, &r);
  if (ovr)
    overflow_handler ();

  return r;
}

/* { dg-final { scan-assembler "subs" } } */
