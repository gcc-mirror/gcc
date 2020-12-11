/* { dg-do compile } */
/* { dg-options "-O2 -muintr" } */
/* { dg-error "'-muintr' not supported for 32-bit code" "" { target ia32 } 0 } */

#include <x86gprintrin.h>

typedef unsigned int uword_t __attribute__ ((mode (__word__)));

void
UINTR_hanlder (struct __uintr_frame *frame, uword_t uirrv)
{
}
