/* PR middle-end/52691 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#include <stdarg.h>

int
foo (int a, ...)
{
  int b = 0, c = 0;
  va_list ap;
  va_start (ap, a);
  if (a > 1)
    b = va_arg (ap, double);
  if (a > 2)
    c = va_arg (ap, long long);
  va_end (ap);
  return a + b + c;
}

/* { dg-final { scan-tree-dump "__builtin_next_arg" "optimized" { target { { i?86-*-* x86_64-*-* } && ia32 } } } } */
/* { dg-final { scan-tree-dump "__builtin_next_arg" "optimized" { target { powerpc*-*-darwin* powerpc*-*-aix* } } } } */
/* { dg-final { scan-tree-dump "__builtin_next_arg" "optimized" { target { powerpc*-*-linux* && lp64 } } } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
