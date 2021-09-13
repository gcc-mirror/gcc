/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -msse2 -mstv -mpreferred-stack-boundary=4 -mno-stackrealign -mregparm=3 -W -mtune=generic" } */
/* { dg-final { scan-assembler "movq\[ \t\]+\[^\n\]*, %xmm" } } */

#include "pr95021-1.c"
