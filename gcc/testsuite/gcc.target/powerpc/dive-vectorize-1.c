/* { dg-do compile } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fdump-tree-vect-details" } */

/* Test if signed/unsigned int extended divisions get vectorized.  */

#include "dive-vectorize-1.h"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
/* { dg-final { scan-assembler-times {\mvdivesw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvdiveuw\M} 1 } } */
