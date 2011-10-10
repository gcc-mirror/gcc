/* { dg-do compile } */
/* { dg-prune-output ".*warning: 'sseregparm' attribute ignored.*" } */
/* { dg-options "-O2 -mfma" } */

/* Test that the compiler properly optimizes floating point multiply
   and add instructions into FMA3 instructions.  */

#define TYPE float

#include "fma_5.h"

/* { dg-final { scan-assembler-times "vfmadd132ss" 8  } } */
/* { dg-final { scan-assembler-times "vfmsub132ss" 8  } } */
/* { dg-final { scan-assembler-times "vfnmadd132ss" 8  } } */
/* { dg-final { scan-assembler-times "vfnmsub132ss" 8  } } */
