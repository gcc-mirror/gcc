/* { dg-do compile } */
/* { dg-options "-O2 -muintr" } */
/* { dg-error "'-muintr' not supported for 32-bit code" "" { target ia32 } 0 } */

#include <x86gprintrin.h>

void
UINTR_hanlder (struct __uintr_frame *frame)
{
}
