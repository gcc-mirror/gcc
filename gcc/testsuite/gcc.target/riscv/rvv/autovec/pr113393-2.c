/* { dg-do run } */
/* { dg-options "-O3 -mrvv-vector-bits=zvl -mrvv-max-lmul=m2" } */
/* { dg-require-effective-target riscv_v } */

__attribute__((noinline, noclone)) static int
bar (const short *a, int len)
{
  int x;
  int x1 = 0;

  for (x = 0; x < len; x++)
    x1 += x * a[x];
  return x1;
}

__attribute__((noinline, noclone)) void
foo (void)
{
  short stuff[9] = {1, 1, 1, 1, 1, 1, 1, 1, 1 };
  if (bar (stuff, 9) != 36)
    __builtin_abort ();
}

int
main ()
{
  foo ();
  return 0;
}
