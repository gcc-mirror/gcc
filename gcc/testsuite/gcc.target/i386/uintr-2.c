/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -muintr -mgeneral-regs-only" } */
/* { dg-final { scan-assembler-times "uiret" "2" } } */

#include <x86gprintrin.h>

void
__attribute__((interrupt))
foo (void *frame)
{
}

void
__attribute__((interrupt))
UINTR_hanlder (struct __uintr_frame *frame)
{
}
