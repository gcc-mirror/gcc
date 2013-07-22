/* { dg-do compile } */
/* { dg-options "-O3 -Wno-attributes -mfpmath=sse -mfma -mtune=generic" } */

/* Test that the compiler properly optimizes floating point multiply
   and add instructions into FMA3 instructions.  */

typedef double adouble __attribute__((aligned(sizeof (double))));
#define TYPE adouble

#include "l_fma_3.h"

/* { dg-final { scan-assembler-times "vfmadd132pd" 4  } } */
/* { dg-final { scan-assembler-times "vfmadd231pd" 4  } } */
/* { dg-final { scan-assembler-times "vfmsub132pd" 4  } } */
/* { dg-final { scan-assembler-times "vfmsub231pd" 4  } } */
/* { dg-final { scan-assembler-times "vfnmadd132pd" 4  } } */
/* { dg-final { scan-assembler-times "vfnmadd231pd" 4  } } */
/* { dg-final { scan-assembler-times "vfnmsub132pd" 4  } } */
/* { dg-final { scan-assembler-times "vfnmsub231pd" 4  } } */
/* { dg-final { scan-assembler-times "vfmadd132sd" 28 } } */
/* { dg-final { scan-assembler-times "vfmadd213sd" 28 } } */
/* { dg-final { scan-assembler-times "vfmsub132sd" 28 } } */
/* { dg-final { scan-assembler-times "vfmsub213sd" 28 } } */
/* { dg-final { scan-assembler-times "vfnmadd132sd" 28 } } */
/* { dg-final { scan-assembler-times "vfnmadd213sd" 28 } } */
/* { dg-final { scan-assembler-times "vfnmsub132sd" 28 } } */
/* { dg-final { scan-assembler-times "vfnmsub213sd" 28 } } */
