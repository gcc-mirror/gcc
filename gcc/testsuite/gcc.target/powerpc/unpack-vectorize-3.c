/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mdejagnu-cpu=power8 -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fdump-tree-vect-details" } */

/* Test if signed int unpack vectorization succeeds.  */

#include "unpack-vectorize-3.h"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-assembler-times {\mvupkhsw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvupklsw\M} 1 } } */
