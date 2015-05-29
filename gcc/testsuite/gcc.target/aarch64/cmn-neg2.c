/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */

extern void abort (void);

/* It's unsafe to use CMN in these comparisons.  */

void __attribute__ ((noinline))
foo_s32 (int a, int b)
{
  if (a < -b)
    abort ();
}

void __attribute__ ((noinline))
foo_s64 (unsigned long long a, unsigned long long b)
{
  if (a > -b)
    abort ();
}


int
main (void)
{
  int a = 30;
  int b = 42;
  foo_s32 (a, b);
  foo_s64 (a, b);
  return 0;
}
/* { dg-final { scan-assembler-not "cmn\t" } } */

