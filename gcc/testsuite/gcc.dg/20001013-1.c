/* ??? It'd be nice to run this for sparc32 as well, if we could know
   for sure that we're on an ultrasparc, rather than an older cpu.  */
/* { dg-do run { target sparcv9-*-* sparc64-*-* } } */
/* { dg-options "-O2 -m32 -mcpu=ultrasparc -mvis" } */

int l;

int baz (double x)
{
  return l == 0;
}

double bar (double x)
{
  return 1.0;
}

double foo (double x)
{
  if (l == -1 || baz (x)) return x;
  if (x < 0.0)
    return bar (x);
  else
    return 0.0;
}

union {
  double d;
  long long l;
} x = { l: 0x7ff8000000000000LL }, y;

main ()
{
  unsigned int fsr = 0;
  __asm __volatile ("ld %0, %%fsr" : : "m" (fsr));
  y.d = foo (x.d);
  __asm __volatile ("st %%fsr, %0" : "=m" (fsr));
  if (x.l != y.l || (fsr & 0x3ff))
    abort ();
  exit (0);
}
