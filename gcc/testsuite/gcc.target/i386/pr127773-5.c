/* PR target/127773 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=x86-64 -ffixed-xmm8" } */
/* The fixed xmm8 recorded by IPA-RA isn't an SSE register written by f.  */
/* { dg-final { scan-assembler-times "vzeroupper" 1 } } */
/* { dg-final { scan-assembler "call\[ \t\]+_?f\\M.*vzeroupper" } } */

#include "pr127773-4.c"
