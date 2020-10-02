/* { dg-options "-O -fno-stack-clash-protection" } */

int foo (void *);

int
f1 (int *x, long y)
{
  return foo (__builtin_alloca (y));
}

int
f2 (int *x, int y)
{
  char a[y + 1][16];
  return foo (a);
}

/* { dg-final { scan-assembler "sub\tsp, sp, x\[0-9\]*\n" } } */
/* { dg-final { scan-assembler "sub\tsp, sp, w\[0-9\]*, sxtw 4\n" { target lp64 } } } */
