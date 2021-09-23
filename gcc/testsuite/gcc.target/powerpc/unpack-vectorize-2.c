/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power7 -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fdump-tree-vect-details" } */

/* Test if unsigned int unpack vectorization succeeds.  V2DImode is
   supported since Power7 so guard it under Power7 and up.  */

#include "unpack-vectorize-2.h"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-assembler-times {\mxxmrghw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxxmrglw\M} 1 } } */
