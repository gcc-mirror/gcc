/* This test has architecture specific function passing details.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-stdarg" } */

#include <stdarg.h>

extern void foo (int, va_list);
extern void bar (int);
struct S1 { int i; double d; int j; double e; } s1;
struct S2 { double d; long i; } s2;
int y;
_Complex int ci;
_Complex double cd;

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
/* { dg-final { scan-tree-dump "f1: va_list escapes 0, needs to save 0 GPR units and 0 FPR units" "stdarg" { target { x86_64-*-* && lp64 } } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes 0, needs to save all GPR units and 1" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes 0, needs to save all GPR units and 0 FPR units" "stdarg" { target s390*-*-linux* } } } */

void
f2 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  while (i-- > 0)
    s2 = va_arg (ap, struct S2);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f2: va_list escapes 0, needs to save all GPR units and all FPR units" "stdarg" { target { x86_64-*-* && lp64 } } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes 0, needs to save all GPR units and 1" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes 0, needs to save all GPR units and 0 FPR units" "stdarg" { target s390*-*-linux* } } } */

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
/* { dg-final { scan-tree-dump "f3: va_list escapes 0, needs to save 0 GPR units and 0 FPR units" "stdarg" { target { x86_64-*-* && lp64 } } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 0, needs to save 32 GPR units and 1" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 0, needs to save 1 GPR units and 0 FPR units" "stdarg" { target s390*-*-linux* } } } */

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
/* { dg-final { scan-tree-dump "f4: va_list escapes 0, needs to save 16 GPR units and 16 FPR units" "stdarg" { target { x86_64-*-* && lp64 } } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 0, needs to save 24 GPR units and 1" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 0, needs to save 2 GPR units and 0 FPR units" "stdarg" { target s390*-*-linux* } } } */

void
f5 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  ci = va_arg (ap, _Complex int);
  ci += va_arg (ap, _Complex int);
  va_end (ap);
  bar (__real__ ci + __imag__ ci);
}
/* { dg-final { scan-tree-dump "f5: va_list escapes 0, needs to save 16 GPR units and 0 FPR units" "stdarg" { target { x86_64-*-* && lp64 } } } } */
/* { dg-final { scan-tree-dump "f5: va_list escapes 0, needs to save 32 GPR units and 1" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f5: va_list escapes 0, needs to save (4|2) GPR units and 0 FPR units" "stdarg" { target s390*-*-linux* } } } */

void
f6 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  ci = va_arg (ap, _Complex int);
  cd = va_arg (ap, _Complex double);
  va_end (ap);
  bar (__real__ ci + __imag__ cd);
}
/* { dg-final { scan-tree-dump "f6: va_list escapes 0, needs to save 8 GPR units and 32 FPR units" "stdarg" { target { x86_64-*-* && lp64 } } } } */
/* { dg-final { scan-tree-dump "f6: va_list escapes 0, needs to save 32 GPR units and 3" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f6: va_list escapes 0, needs to save (3|2) GPR units and 0 FPR units" "stdarg" { target s390*-*-linux* } } } */

void
f7 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  cd = va_arg (ap, _Complex double);
  cd += va_arg (ap, _Complex double);
  va_end (ap);
  bar (__real__ cd + __imag__ cd);
}
/* { dg-final { scan-tree-dump "f7: va_list escapes 0, needs to save 0 GPR units and 64 FPR units" "stdarg" { target { x86_64-*-* && lp64 } } } } */
/* { dg-final { scan-tree-dump "f7: va_list escapes 0, needs to save 32 GPR units and 2" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f7: va_list escapes 0, needs to save 2 GPR units and 0 FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { cleanup-tree-dump "stdarg" } } */
