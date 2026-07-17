/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_compile_ok } */
/* { dg-options "-Wno-psabi -mdejagnu-cpu=future -O2" } */

#include "mma-builtin-2.h"


/* { dg-final { scan-assembler-times {\mlxv\M} 4 } } */
/* { dg-final { scan-assembler-times {\mlxvp\M} 8 } } */
/* { dg-final { scan-assembler-times {\mstxvp\M} 8 } } */
/* { dg-final { scan-assembler-times {\mxvf64ger\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf64gerpp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf64gerpn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf64gernp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxvf64gernn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf64ger\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf64gerpp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf64gerpn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf64gernp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mpmxvf64gernn\M} 1 } } */
