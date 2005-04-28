/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-stdarg" } */

#include <stdarg.h>

/* This can be handled on all arches.  If there is no va_start, registers don't need
   to be saved.  */
void
f1 (int i, ...)
{
}
/* { dg-final { scan-tree-dump "f1: va_list escapes 0, needs to save 0 GPR units and 0 FPR units" "stdarg" } } */
/* { dg-final { cleanup-tree-dump "stdarg" } } */
