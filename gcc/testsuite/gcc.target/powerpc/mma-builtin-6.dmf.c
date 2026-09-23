/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=future -O2" } */

#include "mma-builtin-6.h"

/* { dg-final { scan-assembler-not {\mlxv\M} } } */
/* { dg-final { scan-assembler-not {\mlxvp\M} } } */
/* { dg-final { scan-assembler-not {\mxxmtacc\M} } } */
/* { dg-final { scan-assembler-times {\mxxsetaccz\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 4 } } */
