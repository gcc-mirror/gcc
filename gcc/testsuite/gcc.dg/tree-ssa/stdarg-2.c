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
double d;
va_list gap;
va_list *pap;

void
f1 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f1: va_list escapes 0, needs to save 0 GPR units and 0 FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes 0, needs to save 0 GPR units and 0 FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes 0, needs to save 0 GPR units and 0 FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes 0, needs to save 0 GPR units and 0 FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes 0, needs to save 0 GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes 0, needs to save 0 GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes 0, needs to save 0 GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f2 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  bar (d);
  x = va_arg (ap, long);
  bar (x);
  va_end (ap);
}
/* Assume the counters can be number of registers or bytes on 32-bit
   architecture or bytes on 64-bit architecture.  */
/* { dg-final { scan-tree-dump "f2: va_list escapes 0, needs to save \[148\] GPR units and 0 FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes 0, needs to save \[148\] GPR units and 0 FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes 0, needs to save 8 GPR units and 1" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes 0, needs to save 1 GPR units and 0 FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes 0, needs to save \[148\] GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes 0, needs to save \[148\] GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes 0, needs to save \[148\] GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f3 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  d = va_arg (ap, double);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f3: va_list escapes 0, needs to save 0 GPR units and \[1-9\]\[0-9\]* FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 0, needs to save 0 GPR units and \[1-9\]\[0-9\]* FPR units" "stdarg" { target { powerpc*-*-linux* && { powerpc_fprs && ilp32 } } } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 0, needs to save 0 GPR units and 1 FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 0, needs to save 8 GPR units and 2" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 0, needs to save \[1-9\]\[0-9\]* GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 0, needs to save \[1-9\]\[0-9\]* GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 0, needs to save \[1-9\]\[0-9\]* GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f4 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  x = va_arg (ap, double);
  foo (i, ap);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f4: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f5 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  va_copy (gap, ap);
  bar (i);
  va_end (ap);
  va_end (gap);
}
/* { dg-final { scan-tree-dump "f5: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-tree-dump "f5: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f5: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f5: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f5: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f5: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f5: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f6 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  bar (d);
  va_arg (ap, long);
  va_arg (ap, long);
  x = va_arg (ap, long);
  bar (x);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f6: va_list escapes 0, needs to save (3|12|24) GPR units and 0 FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-tree-dump "f6: va_list escapes 0, needs to save (3|12|24) GPR units and 0 FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f6: va_list escapes 0, needs to save 24 GPR units and 1" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f6: va_list escapes 0, needs to save 3 GPR units and 0 FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f6: va_list escapes 0, needs to save (3|12|24) GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f6: va_list escapes 0, needs to save (3|12|24) GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f6: va_list escapes 0, needs to save (3|12|24) GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f7 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  pap = &ap;
  bar (6);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f7: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-tree-dump "f7: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f7: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f7: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f7: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f7: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f7: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f8 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  pap = &ap;
  bar (d);
  x = va_arg (ap, long);
  bar (x);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f8: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-tree-dump "f8: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f8: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f8: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f8: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f8: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f8: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f9 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  __asm __volatile ("" : "=r" (pap) : "0" (&ap));
  bar (6);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f9: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-tree-dump "f9: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f9: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f9: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f9: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f9: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f9: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f10 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  __asm __volatile ("" : "=r" (pap) : "0" (&ap));
  bar (d);
  x = va_arg (ap, long);
  bar (x);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f10: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-tree-dump "f10: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f10: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f10: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f10: va_list escapes 1, needs to save all GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f10: va_list escapes 1, needs to save all GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f10: va_list escapes 1, needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f11 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  bar (d);
  x = va_arg (ap, long);
  x += va_arg (ap, long);
  x += va_arg (ap, long);
  bar (x);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f11: va_list escapes 0, needs to save (3|12|24) GPR units and 0 FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-tree-dump "f11: va_list escapes 0, needs to save (3|12|24) GPR units and 0 FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f11: va_list escapes 0, needs to save 24 GPR units and 1" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f11: va_list escapes 0, needs to save 3 GPR units and 0 FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f11: va_list escapes 0, needs to save (3|12|24) GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f11: va_list escapes 0, needs to save (3|12|24) GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f11: va_list escapes 0, needs to save (3|12|24) GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f12 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  bar (d);
  va_arg (ap, double);
  va_arg (ap, double);
  x = va_arg (ap, double);
  bar (x);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f12: va_list escapes 0, needs to save 0 GPR units and \[1-9\]\[0-9\]* FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-tree-dump "f12: va_list escapes 0, needs to save 0 GPR units and \[1-9\]\[0-9\]* FPR units" "stdarg" { target { powerpc*-*-linux* && { powerpc_fprs && ilp32 } } } } } */
/* { dg-final { scan-tree-dump "f12: va_list escapes 0, needs to save 24 GPR units and 2" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f12: va_list escapes 0, needs to save 0 GPR units and 3 FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f12: va_list escapes 0, needs to save \[1-9]\[0-9\]* GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f12: va_list escapes 0, needs to save \[1-9]\[0-9\]* GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f12: va_list escapes 0, needs to save \[1-9]\[0-9\]* GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f13 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  bar (d);
  x = va_arg (ap, double);
  x += va_arg (ap, double);
  x += va_arg (ap, double);
  bar (x);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f13: va_list escapes 0, needs to save 0 GPR units and \[1-9\]\[0-9\]* FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-tree-dump "f13: va_list escapes 0, needs to save 0 GPR units and \[1-9\]\[0-9\]* FPR units" "stdarg" { target { powerpc*-*-linux* && { powerpc_fprs && ilp32 } } } } } */
/* { dg-final { scan-tree-dump "f13: va_list escapes 0, needs to save 24 GPR units and 2" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f13: va_list escapes 0, needs to save 0 GPR units and 3 FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f13: va_list escapes 0, needs to save \[1-9]\[0-9\]* GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f13: va_list escapes 0, needs to save \[1-9]\[0-9\]* GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f13: va_list escapes 0, needs to save \[1-9]\[0-9\]* GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f14 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  bar (d);
  x = va_arg (ap, double);
  x += va_arg (ap, long);
  x += va_arg (ap, double);
  bar (x);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f14: va_list escapes 0, needs to save \[148\] GPR units and \[1-9\]\[0-9\]* FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-tree-dump "f14: va_list escapes 0, needs to save \[148\] GPR units and \[1-9\]\[0-9\]* FPR units" "stdarg" { target { powerpc*-*-linux* && { powerpc_fprs && ilp32 } } } } } */
/* { dg-final { scan-tree-dump "f14: va_list escapes 0, needs to save 24 GPR units and 3" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f14: va_list escapes 0, needs to save 1 GPR units and 2 FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f14: va_list escapes 0, needs to save \[1-9]\[0-9\]* GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f14: va_list escapes 0, needs to save \[1-9]\[0-9\]* GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f14: va_list escapes 0, needs to save \[1-9]\[0-9\]* GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

inline void __attribute__((always_inline))
f15_1 (va_list ap)
{
  x = va_arg (ap, double);
  x += va_arg (ap, long);
  x += va_arg (ap, double);
}

void
f15 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  f15_1 (ap);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f15: va_list escapes 0, needs to save \[148\] GPR units and \[1-9\]\[0-9\]* FPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && lp64 } } } } */
/* { dg-final { scan-tree-dump "f15: va_list escapes 0, needs to save \[148\] GPR units and \[1-9\]\[0-9\]* FPR units" "stdarg" { target { powerpc*-*-linux* && { powerpc_fprs && ilp32 } } } } } */
/* { dg-final { scan-tree-dump "f15: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f15: va_list escapes 0, needs to save 1 GPR units and 2 FPR units" "stdarg" { target s390*-*-linux* } } } */
/* { dg-final { scan-tree-dump-not "f15: va_list escapes 0, needs to save 0 GPR units" "stdarg" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } } */
/* { dg-final { scan-tree-dump-not "f15: va_list escapes 0, needs to save 0 GPR units" "stdarg" { target ia64-*-* } } } */
/* { dg-final { scan-tree-dump-not "f15: va_list escapes 0, needs to save 0 GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */
/* { dg-final { cleanup-tree-dump "stdarg" } } */
