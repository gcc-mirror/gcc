/* First dg-final line after each function is for architectures that use
   a struct {...} va_list[1] with separate GPR and FPR counters in the
   structure.  Second dg-final line is for architectures that use void *
   or char * va_list.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-stdarg" } */

#include <stdarg.h>

extern void foo (int, va_list);
extern void bar (int);
long x;
va_list gap;

/* If va_list is not local variable, it escapes the function.  */
void
f1 (int i, ...)
{
  va_start (gap, i);
  x = va_arg (gap, long);
  va_end (gap);
}
/* { dg-final { scan-tree-dump "f1: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ia32 } } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f2 (int i, ...)
{
  va_start (gap, i);
  bar (i);
  va_end (gap);
}
/* { dg-final { scan-tree-dump "f2: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ia32 } } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

/* tree-stdarg.c only handles va_list variables, not arrays of them or
   va_list fields embedded in structures.  */
void
f3 (int i, ...)
{
  va_list aps[10];
  va_start (aps[4], i);
  x = va_arg (aps[4], long);
  va_end (aps[4]);
}
/* { dg-final { scan-tree-dump "f3: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ia32 } } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f4 (int i, ...)
{
  va_list aps[10];
  va_start (aps[4], i);
  bar (i);
  va_end (aps[4]);
}
/* { dg-final { scan-tree-dump "f4: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ia32 } } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f5 (int i, ...)
{
  va_list aps[10];
  va_start (aps[4], i);
  foo (i, aps[4]);
  va_end (aps[4]);
}
/* { dg-final { scan-tree-dump "f5: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-tree-dump "f5: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f5: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f5: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f5: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ia32 } } } } */
/* { dg-final { scan-tree-dump "f5: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f5: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

struct A { int i; va_list g; va_list h[2]; };

void
f6 (int i, ...)
{
  struct A a;
  va_start (a.g, i);
  x = va_arg (a.g, long);
  va_end (a.g);
}
/* { dg-final { scan-tree-dump "f6: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-tree-dump "f6: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f6: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f6: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f6: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ia32 } } } } */
/* { dg-final { scan-tree-dump "f6: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f6: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f7 (int i, ...)
{
  struct A a;
  va_start (a.g, i);
  bar (i);
  va_end (a.g);
}
/* { dg-final { scan-tree-dump "f7: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-tree-dump "f7: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f7: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f7: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f7: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ia32 } } } } */
/* { dg-final { scan-tree-dump "f7: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f7: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f8 (int i, ...)
{
  struct A a;
  va_start (a.g, i);
  foo (i, a.g);
  va_end (a.g);
}
/* { dg-final { scan-tree-dump "f8: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-tree-dump "f8: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f8: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f8: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f8: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ia32 } } } } */
/* { dg-final { scan-tree-dump "f8: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f8: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f10 (int i, ...)
{
  struct A a;
  va_start (a.h[1], i);
  x = va_arg (a.h[1], long);
  va_end (a.h[1]);
}
/* { dg-final { scan-tree-dump "f10: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-tree-dump "f10: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f10: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f10: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f10: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ia32 } } } } */
/* { dg-final { scan-tree-dump "f10: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f10: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f11 (int i, ...)
{
  struct A a;
  va_start (a.h[1], i);
  bar (i);
  va_end (a.h[1]);
}
/* { dg-final { scan-tree-dump "f11: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-tree-dump "f11: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f11: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f11: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f11: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ia32 } } } } */
/* { dg-final { scan-tree-dump "f11: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f11: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f12 (int i, ...)
{
  struct A a;
  va_start (a.h[1], i);
  foo (i, a.h[1]);
  va_end (a.h[1]);
}
/* { dg-final { scan-tree-dump "f12: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } } } */
/* { dg-final { scan-tree-dump "f12: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f12: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f12: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f12: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ia32 } } } } */
/* { dg-final { scan-tree-dump "f12: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f12: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */
/* { dg-final { cleanup-tree-dump "stdarg" } } */
