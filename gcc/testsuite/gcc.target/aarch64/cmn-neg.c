/* { dg-do run } */
/* { dg-options "-O2 --save-temps" } */

extern void abort (void);

void __attribute__ ((noinline))
foo_s32 (int a, int b)
{
  if (a == -b)
    abort ();
}
/* { dg-final { scan-assembler "cmn\tw\[0-9\]" } } */

void __attribute__ ((noinline))
foo_s64 (long long a, long long b)
{
  if (a == -b)
    abort ();
}
/* { dg-final { scan-assembler "cmn\tx\[0-9\]" } } */


int
main (void)
{
  int a = 30;
  int b = 42;
  foo_s32 (a, b);
  foo_s64 (a, b);
  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
