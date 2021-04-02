/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -muintr -mgeneral-regs-only" } */
/* { dg-final { scan-assembler-times "uiret" "2" } } */
/* { dg-final { scan-assembler-times "add\[lq]\[ \t]\+\\\$8, %\[er\]sp" "2" } } */

#include <x86gprintrin.h>

typedef unsigned int uword_t __attribute__ ((mode (__word__)));

void
__attribute__((interrupt))
foo (void *frame, uword_t uirrv)
{
}

void
__attribute__((interrupt))
UINTR_hanlder (struct __uintr_frame *frame, uword_t uirrv)
{
}
