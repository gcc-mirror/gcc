/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -muintr" } */

#include <x86gprintrin.h>

void __attribute__ ((interrupt))
UINTR_handler (struct __uintr_frame *p)
{ /* { dg-message "SSE instructions aren't allowed in an interrupt service routine" }  */
}
