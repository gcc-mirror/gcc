/* This test is only for architectures that save general and floating point
   registers separately in stdarg functions.  */
/* { dg-do compile { target x86_64-*-* powerpc-*-* } } */
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
/* { dg-final { scan-tree-dump "f1: va_list escapes 0, needs to save 0 GPR units and 0 FPR units" "stdarg" } } */

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
/* { dg-final { scan-tree-dump "f2: va_list escapes 0, needs to save \[148\] GPR units and 0 FPR units" "stdarg" } } */

void
f3 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  d = va_arg (ap, double);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f3: va_list escapes 0, needs to save 0 GPR units and \[1-9\]\[0-9\]* FPR units" "stdarg" } } */

void
f4 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  x = va_arg (ap, double);
  foo (i, ap);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f4: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" } } */

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
/* { dg-final { scan-tree-dump "f5: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" } } */

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
/* { dg-final { scan-tree-dump "f6: va_list escapes 0, needs to save (3|12|24) GPR units and 0 FPR units" "stdarg" } } */

void
f7 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  pap = &ap;
  bar (6);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f7: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" } } */

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
/* { dg-final { scan-tree-dump "f8: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" } } */

void
f9 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  __asm __volatile ("" : "=r" (pap) : "0" (&ap));
  bar (6);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f9: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" } } */

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
/* { dg-final { scan-tree-dump "f10: va_list escapes 1, needs to save all GPR units and all FPR units" "stdarg" } } */
