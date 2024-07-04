/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

/* This file just generates calls to the various builtins and verifies the
   expected number of instructions for each builtin were generated.  */

#include "vsx-vector-6-func-2lop.h"

/* { dg-final { scan-assembler-times {\mxxland\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxlandc\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxlnor\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxlxor\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxlor\M} 2 } } */
