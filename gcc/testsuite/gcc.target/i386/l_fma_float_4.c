/* { dg-do compile } */
/* { dg-options "-O3 -Wno-attributes -mfpmath=sse -mfma -mtune=generic" } */

/* Test that the compiler properly optimizes floating point multiply
   and add instructions into FMA3 instructions.  */

#define TYPE float

#include "l_fma_4.h"

/* { dg-final { scan-assembler-times "vfmadd132ps" 8  } } */
/* { dg-final { scan-assembler-times "vfmsub132ps" 8  } } */
/* { dg-final { scan-assembler-times "vfnmadd132ps" 8  } } */
/* { dg-final { scan-assembler-times "vfnmsub132ps" 8  } } */
/* { dg-final { scan-assembler-times "vfmadd132ss" 120  } } */
/* { dg-final { scan-assembler-times "vfmsub132ss" 120  } } */
/* { dg-final { scan-assembler-times "vfnmadd132ss" 120  } } */
/* { dg-final { scan-assembler-times "vfnmsub132ss" 120  } } */
