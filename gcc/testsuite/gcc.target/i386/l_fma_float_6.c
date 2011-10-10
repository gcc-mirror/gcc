/* { dg-do compile } */
/* { dg-prune-output ".*warning: 'sseregparm' attribute ignored.*" } */
/* { dg-options "-O2 -mfma" } */

/* Test that the compiler properly optimizes floating point multiply
   and add instructions into FMA3 instructions.  */

/* { dg-options "-O3 -mfma" } */


#define TYPE float

#include "l_fma_6.h"

/* { dg-final { scan-assembler-times "vfmadd132ps" 8  } } */
/* { dg-final { scan-assembler-times "vfmsub132ps" 8  } } */
/* { dg-final { scan-assembler-times "vfnmadd132ps" 8  } } */
/* { dg-final { scan-assembler-times "vfnmsub132ps" 8  } } */
/* { dg-final { scan-assembler-times "vfmadd132ss" 8  } } */
/* { dg-final { scan-assembler-times "vfmsub132ss" 8  } } */
/* { dg-final { scan-assembler-times "vfnmadd132ss" 8  } } */
/* { dg-final { scan-assembler-times "vfnmsub132ss" 8  } } */
