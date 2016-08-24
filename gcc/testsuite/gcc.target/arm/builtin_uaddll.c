/* { dg-do compile } */
/* { dg-options "-O2" }  */
/* { dg-require-effective-target arm32 } */
extern void overflow_handler ();

unsigned long long overflow_add (unsigned long long x, unsigned long long y)
{
  unsigned long long r;

  int ovr = __builtin_uaddll_overflow (x, y, &r);
  if (ovr)
    overflow_handler ();

  return r;
}

/* { dg-final { scan-assembler "adds" } } */
/* { dg-final { scan-assembler "adcs" } } */
