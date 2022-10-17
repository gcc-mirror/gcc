/* { dg-do compile } */
/* What we scan for are only supported on 64-bit env.  */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -ftree-vectorize -fno-vect-cost-model -fno-unroll-loops -fdump-tree-vect-details" } */

/* Test if some Power10 built-in functions get vectorized.  */

#include "p10-bifs-vectorize-1.h"

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 5 "vect" } } */
/* { dg-final { scan-assembler-times {\mvcfuged\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvclzdm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvctzdm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvpdepd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvpextd\M} 1 } } */
