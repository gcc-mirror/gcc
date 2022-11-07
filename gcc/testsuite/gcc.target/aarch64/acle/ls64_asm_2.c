/* { dg-do compile } */
/* { dg-options "-O" } */

#pragma GCC target "+ls64+nofp"

#include "ls64_asm.c"

/* { dg-final { scan-assembler-times {\tldp\t} 12 } } */
/* { dg-final { scan-assembler-times {\tstp\t} 4 } } */
