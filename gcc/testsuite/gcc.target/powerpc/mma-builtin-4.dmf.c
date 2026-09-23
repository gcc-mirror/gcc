/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=future -O2" } */

#include "mma-builtin-4.h"

/* { dg-final { scan-assembler-times {\mlxv\M} 6 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstxv\M} 4 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 3 } } */
