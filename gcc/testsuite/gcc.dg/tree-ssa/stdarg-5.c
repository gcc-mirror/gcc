/* This test is specific to x86-64 function passing.  */
/* { dg-do compile { target x86_64-*-* } } */
/* { dg-options "-O2 -fdump-tree-stdarg" } */

#include <stdarg.h>

extern void foo (int, va_list);
extern void bar (int);
struct S1 { int i; double d; int j; double e; } s1;
struct S2 { double d; long i; } s2;
int y;

/* Here va_arg can be executed more than once for one va_start.  */
void
f1 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  while (i-- > 0)
    s1 = va_arg (ap, struct S1);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f1: va_list escapes 0, needs to save 0 GPR units and 0 FPR units" "stdarg" } } */

void
f2 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  while (i-- > 0)
    s2 = va_arg (ap, struct S2);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f2: va_list escapes 0, needs to save all GPR units and all FPR units" "stdarg" } } */

/* Here va_arg can be executed at most as many times as va_start.  */
void
f3 (int i, ...)
{
  va_list ap;
  int j = i;
  while (j-- > 0)
    {
      va_start (ap, i);
      s1 = va_arg (ap, struct S1);
      va_end (ap);
      bar (s1.i);
    }
}
/* { dg-final { scan-tree-dump "f3: va_list escapes 0, needs to save 0 GPR units and 0 FPR units" "stdarg" } } */

void
f4 (int i, ...)
{
  va_list ap;
  int j = i;
  while (j-- > 0)
    {
      va_start (ap, i);
      s2 = va_arg (ap, struct S2);
      y = va_arg (ap, int);
      va_end (ap);
      bar (s2.i);
    }
}
/* { dg-final { scan-tree-dump "f4: va_list escapes 0, needs to save 16 GPR units and 16 FPR units" "stdarg" } } */
