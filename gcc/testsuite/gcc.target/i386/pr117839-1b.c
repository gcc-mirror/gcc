/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3" } */
/* { dg-final { scan-assembler-times "xor\[a-z\]*\[\t \]*%xmm\[0-9\]\+,\[^,\]*" 1 } } */

#include "pr117839-1a.c"
