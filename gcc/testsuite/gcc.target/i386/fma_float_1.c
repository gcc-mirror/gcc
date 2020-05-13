/* { dg-do compile } */
/* { dg-options "-O2 -Wno-attributes -mfpmath=sse -mfma" } */

/* Test that the compiler properly optimizes floating point multiply
   and add instructions into FMA3 instructions.  */

#define TYPE float

#include "fma_1.h"

/* { dg-final { scan-assembler-times "vfmadd132ss" 8  } } */
/* { dg-final { scan-assembler-times "vfmadd231ss" 4  } } */
/* { dg-final { scan-assembler-times "vfmsub132ss" 8  } } */
/* { dg-final { scan-assembler-times "vfmsub231ss" 4  } } */
/* { dg-final { scan-assembler-times "vfnmadd231ss" 4  } } */
/* { dg-final { scan-assembler-times "vfnmsub231ss" 4  } } */
