/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -muintr" } */

#include <x86gprintrin.h>

typedef unsigned int uword_t __attribute__ ((mode (__word__)));

void __attribute__ ((interrupt))
UINTR_handler (struct __uintr_frame *p, uword_t uirrv)
{ /* { dg-message "SSE instructions aren't allowed in an exception service routine" }  */
}
