/* { dg-do compile } */
/* { dg-options "-O2 -mforce-indirect-call -mcmodel=medium" } */
/* { dg-require-effective-target lp64 } */
/* { dg-final { scan-assembler-times "(?:call|jmp)\[ \\t\]+\\*%" 3 } } */

#include "force-indirect-call-1.c"
