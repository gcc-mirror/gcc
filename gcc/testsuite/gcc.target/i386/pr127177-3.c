/* PR target/127177 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-avx -ftrapping-math -fdump-tree-vect-details" } */
/* { dg-additional-options "-mfpmath=sse" { target ia32 } } */

/* With SSE2 only the unordered compares except UNEQ should not be vectorized under
   -ftrapping-math, as they may raise FE_INVALID.  */

#include "pr127177-1.c"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-assembler-not "cmpnltps" } } */
/* { dg-final { scan-assembler-not "cmpnleps" } } */
/* { dg-final { scan-assembler "ucomiss" } } */
