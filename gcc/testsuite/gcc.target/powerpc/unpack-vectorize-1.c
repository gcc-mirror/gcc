/* { dg-do compile } */
/* { dg-options "-maltivec -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fdump-tree-vect-details" } */
/* { dg-require-effective-target powerpc_altivec } */

/* Test if unpack vectorization succeeds for type signed/unsigned
   short and char.  */

#include "unpack-vectorize-1.h"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 4 "vect" } } */
/* { dg-final { scan-assembler-times {\mvupkhsb\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvupklsb\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvupkhsh\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvupklsh\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvmrghb\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvmrglb\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvmrghh\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvmrglh\M} 2 } } */
