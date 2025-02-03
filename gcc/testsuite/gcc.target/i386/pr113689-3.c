/* { dg-do run { target { lp64 && fpic } } } */
/* { dg-options "-O2 -fpic -fprofile -mcmodel=large" } */
/* { dg-require-profiling "-fprofile" } */
/* { dg-skip-if "PR90698" { *-*-darwin* } } */
/* { dg-skip-if "PR113909" { *-*-solaris2* } } */

#include <stdarg.h>

__attribute__((noipa))
void
bar (char *x, char *y, int *z)
{
  x[0] = 42;
  y[0] = 42;
  if (z[0] != 16)
    __builtin_abort ();
}

__attribute__((noipa))
void 
foo (int a1, int a2, int a3, int a4, int a5, int a6, int z, ...)
{
  typedef char B[32];
  B b __attribute__((aligned (32)));
  va_list ap;
  va_start (ap, z);
  double x = va_arg (ap, double);
  if (x > 40.0)
    __builtin_abort ();
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
  bar (&b[0], __builtin_alloca (z), &z);
  va_end (ap);
}

int
main ()
{
  foo (1, 2, 3, 4, 5, 6, 16, 38.0);
  return 0;
}
