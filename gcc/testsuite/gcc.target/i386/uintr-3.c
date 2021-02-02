/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -muintr" } */
/* { dg-final { scan-assembler "uiret" } } */
/* { dg-final { scan-assembler "add\[lq]\[ \t]\+\\\$8, %\[er\]sp" } } */

#include <x86gprintrin.h>

typedef unsigned int uword_t __attribute__ ((mode (__word__)));

void __attribute__ ((target("general-regs-only"), interrupt))
UINTR_handler (struct __uintr_frame *p, uword_t uirrv)
{
}
