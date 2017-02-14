/* { dg-do compile } */
/* { dg-options "-O2" }  */
/* { dg-require-effective-target arm32 } */
extern void overflow_handler ();

unsigned long overflow_add (unsigned long x, unsigned long y)
{
  unsigned long r;

  int ovr = __builtin_uaddl_overflow (x, y, &r);
  if (ovr)
    overflow_handler ();

  return r;
}

/* { dg-final { scan-assembler "adds" } } */
