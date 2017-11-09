/* { dg-do compile } */
/* { dg-options "-O2 -mforce-indirect-call -mcmodel=medium" } */
/* { dg-final { scan-assembler-times "call\[ \\t\]+\\*%" 2 } } */
/* { dg-final { scan-assembler-times "jmp\[ \\t\]+\\*%" 1 } } */
#include "force-indirect-call-1.c"
