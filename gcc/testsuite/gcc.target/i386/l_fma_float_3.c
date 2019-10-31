/* { dg-do compile } */
/* { dg-options "-O3 -Wno-attributes -mfpmath=sse -mfma -mtune=generic -mno-fma4" } */
/* Disabling epilogues until we find a better way to deal with scans.  */
/* { dg-additional-options "--param vect-epilogues-nomask=0" } */

/* Test that the compiler properly optimizes floating point multiply
   and add instructions into FMA3 instructions.  */

#define TYPE float

#include "l_fma_3.h"

/* { dg-final { scan-assembler-times "vfmadd\[123\]+ps" 8 } } */
/* { dg-final { scan-assembler-times "vfmsub\[123\]+ps" 8 } } */
/* { dg-final { scan-assembler-times "vfnmadd\[123\]+ps" 8 } } */
/* { dg-final { scan-assembler-times "vfnmsub\[123\]+ps" 8 } } */
/* { dg-final { scan-assembler-times "vfmadd\[123\]+ss" 64 } } */
/* { dg-final { scan-assembler-times "vfmsub\[123\]+ss" 64 } } */
/* { dg-final { scan-assembler-times "vfnmadd\[123\]+ss" 64 } } */
/* { dg-final { scan-assembler-times "vfnmsub\[123\]+ss" 64 } } */
