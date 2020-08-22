/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -msse2 -mstv -mregparm=3 -W" } */
/* { dg-final { scan-assembler "movq\[ \t\]+\[^\n\]*, %xmm" } } */

#include "pr95021-1.c"
