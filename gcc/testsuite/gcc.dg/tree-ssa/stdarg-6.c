/* PR tree-optimization/56205 */
/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-stdarg" } */

#include <stdarg.h>

int a, b;
char c[128];

static inline void
foo (int x, char const *y, va_list z)
{
  __builtin_printf ("%s %d %s", x ? "" : "foo", ++a, (y && *y) ? "bar" : "");
  if (y && *y)
    __builtin_vprintf (y, z);
}

void
bar (int x, char const *y, ...)
{
  va_list z;
  va_start (z, y);
  if (!x && *c == '\0')
    ++b;
  foo (x, y, z);
  va_end (z);
}

/* { dg-final { scan-tree-dump "bar: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-tree-dump "bar: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "bar: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "bar: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "bar: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ia32 } } } } */
/* { dg-final { scan-tree-dump "bar: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "bar: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */
