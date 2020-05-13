/* { dg-do compile } */
/* { dg-options "-O2 -Wno-attributes -mfpmath=sse -mfma" } */

/* Test that the compiler properly optimizes floating point multiply
   and add instructions into FMA3 instructions.  */

#define TYPE double

#include "fma_3.h"

/* { dg-final { scan-assembler-times "vfmadd\[123\]+sd" 12  } } */
/* { dg-final { scan-assembler-times "vfmsub\[132\]+sd" 12  } } */
/* { dg-final { scan-assembler-times "vfnmadd\[132\]+sd" 4  } } */
/* { dg-final { scan-assembler-times "vfnmsub\[132\]+sd" 4  } } */
