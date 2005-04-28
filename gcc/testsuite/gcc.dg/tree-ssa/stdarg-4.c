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

/* Here va_arg can be executed more than once for one va_start.  All GPR
   registers needs to be saved.  */
void
f1 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  while (i-- > 0)
    x = va_arg (ap, long);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f1: va_list escapes 0, needs to save all GPR units and 0 FPR units" "stdarg" { target { x86_64-*-* && lp64 } } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes 0, needs to save all GPR units and 0 FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes 0, needs to save all GPR units and 1" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes \[01\], needs to save all GPR units" "stdarg" { target i?86-*-* ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f1: va_list escapes \[01\], needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f2 (int i, ...)
{
  va_list ap;
  va_start (ap, i);
  while (i-- > 0)
    d = va_arg (ap, double);
  va_end (ap);
}
/* { dg-final { scan-tree-dump "f2: va_list escapes 0, needs to save 0 GPR units and all FPR units" "stdarg" { target { x86_64-*-* && lp64 } } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes 0, needs to save 0 GPR units and all FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes 0, needs to save all GPR units and 2" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes \[01\], needs to save all GPR units" "stdarg" { target i?86-*-* ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f2: va_list escapes \[01\], needs to save all GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

/* Here va_arg can be executed at most as many times as va_start.
   Only one GPR needs to be saved.  */
void
f3 (int i, ...)
{
  va_list ap;
  int j = i;
  while (j-- > 0)
    {
      va_start (ap, i);
      x = va_arg (ap, long);
      va_end (ap);
      bar (x);
    }
}
/* { dg-final { scan-tree-dump "f3: va_list escapes 0, needs to save \[148\] GPR units and 0 FPR units" "stdarg" { target { x86_64-*-* && lp64 } } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 0, needs to save \[148\] GPR units and 0 FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 0, needs to save 8 GPR units and 1" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 0, needs to save \[148\] GPR units" "stdarg" { target i?86-*-* ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f3: va_list escapes 0, needs to save \[148\] GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */

void
f4 (int i, ...)
{
  va_list ap;
  int j = i;
  while (j-- > 0)
    {
      va_start (ap, i);
      d = va_arg (ap, double);
      va_end (ap);
      bar (d + 2.5);
    }
}
/* { dg-final { scan-tree-dump "f4: va_list escapes 0, needs to save 0 GPR units and \[1-9\]\[0-9\]* FPR units" "stdarg" { target { x86_64-*-* && lp64 } } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 0, needs to save 0 GPR units and \[1-9\]\[0-9\]* FPR units" "stdarg" { target { powerpc*-*-linux* && ilp32 } } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 0, needs to save 8 GPR units and 2" "stdarg" { target alpha*-*-linux* } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 0, needs to save \[148\] GPR units" "stdarg" { target i?86-*-* ia64-*-* } } } */
/* { dg-final { scan-tree-dump "f4: va_list escapes 0, needs to save \[148\] GPR units" "stdarg" { target { powerpc*-*-* && lp64 } } } } */
/* { dg-final { cleanup-tree-dump "stdarg" } } */
