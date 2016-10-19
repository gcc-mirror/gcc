/* { dg-do compile } */
/* { dg-options "-O3 -Wno-attributes -mfpmath=sse -mfma -mtune=generic -mno-fma4" } */

/* Test that the compiler properly optimizes floating point multiply
   and add instructions into FMA3 instructions.  */

#define TYPE float

#include "l_fma_4.h"

/* { dg-final { scan-assembler-times "vfmadd\[123\]+ps" 8 } } */
/* { dg-final { scan-assembler-times "vfmsub\[123\]+ps" 8 } } */
/* { dg-final { scan-assembler-times "vfnmadd\[123\]+ps" 8 } } */
/* { dg-final { scan-assembler-times "vfnmsub\[123\]+ps" 8 } } */
/* { dg-final { scan-assembler-times "vfmadd\[123\]+ss" 176 } } */
/* { dg-final { scan-assembler-times "vfmsub\[123\]+ss" 176 } } */
/* { dg-final { scan-assembler-times "vfnmadd\[123\]+ss" 176 } } */
/* { dg-final { scan-assembler-times "vfnmsub\[123\]+ss" 176 } } */
