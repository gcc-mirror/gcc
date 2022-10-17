/* { dg-do compile } */
/* We scan for vdive*d which are only supported on 64-bit env.  */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fdump-tree-vect-details" } */

/* Test if signed/unsigned long long extended divisions get vectorized.  */

#include "dive-vectorize-2.h"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
/* { dg-final { scan-assembler-times {\mvdivesd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvdiveud\M} 1 } } */
