/* { dg-do compile } */
/* { dg-options "-O3 -Wno-attributes -mfpmath=sse -mfma -mtune=generic" } */

/* Test that the compiler properly optimizes floating point multiply
   and add instructions into FMA3 instructions.  */

#define TYPE float

#include "l_fma_1.h"

/* { dg-final { scan-assembler-times "vfmadd132ps" 4  } } */
/* { dg-final { scan-assembler-times "vfmadd231ps" 4  } } */
/* { dg-final { scan-assembler-times "vfmsub132ps" 4  } } */
/* { dg-final { scan-assembler-times "vfmsub231ps" 4  } } */
/* { dg-final { scan-assembler-times "vfnmadd132ps" 4  } } */
/* { dg-final { scan-assembler-times "vfnmadd231ps" 4  } } */
/* { dg-final { scan-assembler-times "vfnmsub132ps" 4  } } */
/* { dg-final { scan-assembler-times "vfnmsub231ps" 4  } } */
/* { dg-final { scan-assembler-times "vfmadd132ss" 32  } } */
/* { dg-final { scan-assembler-times "vfmadd213ss" 32  } } */
/* { dg-final { scan-assembler-times "vfmsub132ss" 32  } } */
/* { dg-final { scan-assembler-times "vfmsub213ss" 32  } } */
/* { dg-final { scan-assembler-times "vfnmadd132ss" 32  } } */
/* { dg-final { scan-assembler-times "vfnmadd213ss" 32  } } */
/* { dg-final { scan-assembler-times "vfnmsub132ss" 32  } } */
/* { dg-final { scan-assembler-times "vfnmsub213ss" 32  } } */
