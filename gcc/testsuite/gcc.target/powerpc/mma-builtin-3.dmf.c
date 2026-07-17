/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=future -O2" } */

#include "mma-builtin-3.h"

/* { dg-final { scan-assembler-times {\mlxv\M} 2 } } */
/* { dg-final { scan-assembler-times {\mstxv\M} 2 } } */
/* { dg-final { scan-assembler-not {\mlxvp\M} } } */
/* { dg-final { scan-assembler-not {\mstxvp\M} } } */
/* { dg-final { scan-assembler-times {\mxvcvspbf16\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvcvbf16spn\M} 1 } } */
