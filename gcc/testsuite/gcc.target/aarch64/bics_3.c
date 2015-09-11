/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */

extern void abort (void);

int __attribute__ ((noinline))
bics_si_test (int a, int b)
{
  if (a & ~b)
    return 1;
  else
    return 0;
}

int __attribute__ ((noinline))
bics_si_test2 (int a, int b)
{
  if (a & ~ (b << 2))
    return 1;
  else
    return 0;
}

typedef long long s64;

int __attribute__ ((noinline))
bics_di_test (s64 a, s64 b)
{
  if (a & ~b)
    return 1;
  else
    return 0;
}

int __attribute__ ((noinline))
bics_di_test2 (s64 a, s64 b)
{
  if (a & ~(b << 2))
    return 1;
  else
    return 0;
}

int
main (void)
{
  int a = 5;
  int b = 5;
  int c = 20;
  s64 d = 5;
  s64 e = 5;
  s64 f = 20;
  if (bics_si_test (a, b))
    abort ();
  if (bics_si_test2 (c, b))
    abort ();
  if (bics_di_test (d, e))
    abort ();
  if (bics_di_test2 (f, e))
    abort ();
  return 0;
}

/* { dg-final { scan-assembler-times "bics\twzr, w\[0-9\]+, w\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-times "bics\twzr, w\[0-9\]+, w\[0-9\]+, lsl 2" 1 } } */
/* { dg-final { scan-assembler-times "bics\txzr, x\[0-9\]+, x\[0-9\]+" 2 } } */
/* { dg-final { scan-assembler-times "bics\txzr, x\[0-9\]+, x\[0-9\]+, lsl 2" 1 } } */

