/* { dg-do compile } */
/* { dg-options "-O2" }  */
/* { dg-require-effective-target arm32 } */
extern void overflow_handler ();

long overflow_add (long x, long y)
{
  long r;

  int ovr = __builtin_saddl_overflow (x, y, &r);
  if (ovr)
    overflow_handler ();

  return r;
}

/* { dg-final { scan-assembler "adds" } } */
