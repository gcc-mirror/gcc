/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -muintr" } */
/* { dg-final { scan-assembler "uiret" } } */
#include <x86gprintrin.h>

void __attribute__ ((target("general-regs-only"), interrupt))
UINTR_handler (struct __uintr_frame *p)
{
}
