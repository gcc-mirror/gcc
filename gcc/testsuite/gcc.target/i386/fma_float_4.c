/* { dg-do compile } */
/* { dg-options "-O2 -Wno-attributes -mfpmath=sse -mfma" } */

/* Test that the compiler properly optimizes floating point multiply
   and add instructions into FMA3 instructions.  */

#define TYPE float

#include "fma_4.h"

/* { dg-final { scan-assembler-times "vfmadd132ss" 12  } } */
/* { dg-final { scan-assembler-times "vfmsub132ss" 12  } } */
/* { dg-final { scan-assembler-times "vfnmadd132ss" 4  } } */
/* { dg-final { scan-assembler-times "vfnmsub132ss" 4  } } */
