/* { dg-do run { target { lp64 && fpic } } } */
/* { dg-options "-O2 -fpic -fprofile -mcmodel=large" } */
/* { dg-skip-if "PR90698" { *-*-darwin* } } */
/* { dg-skip-if "PR113909" { *-*-solaris2* } } */

__attribute__((noipa))
void
bar (int a1, int a2, int a3, int a4, int a5, int a6,
     char *x, char *y, int *z)
{
  if (a1 != 1)
    __builtin_abort ();
  if (a2 != 2)
    __builtin_abort ();
  if (a3 != 3)
    __builtin_abort ();
  if (a4 != 4)
    __builtin_abort ();
  if (a5 != 5)
    __builtin_abort ();
  if (a6 != 6)
    __builtin_abort ();
  x[0] = 42;
  y[0] = 42;
  if (z[0] != 16)
    __builtin_abort ();
}

__attribute__((noipa))
void 
foo (int c, int d, int e, int f, int g, int h, int z)
{
  typedef char B[32];
  B b __attribute__((aligned (32)));
  bar (c, d, e, f, g, h, &b[0], __builtin_alloca (z), &z);
}

int
main ()
{
  foo (1, 2, 3, 4, 5, 6, 16);
  return 0;
}
