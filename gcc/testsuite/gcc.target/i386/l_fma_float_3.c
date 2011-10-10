/* { dg-do compile } */
/* { dg-prune-output ".*warning: 'sseregparm' attribute ignored.*" } */
/* { dg-options "-O2 -mfma" } */

/* Test that the compiler properly optimizes floating point multiply
   and add instructions into FMA3 instructions.  */

/* { dg-options "-O3 -mfma" } */


#define TYPE float

#include "l_fma_3.h"

/* { dg-final { scan-assembler-times "vfmadd132ps" 4  } } */
/* { dg-final { scan-assembler-times "vfmadd231ps" 4  } } */
/* { dg-final { scan-assembler-times "vfmsub132ps" 4  } } */
/* { dg-final { scan-assembler-times "vfmsub231ps" 4  } } */
/* { dg-final { scan-assembler-times "vfnmadd132ps" 4  } } */
/* { dg-final { scan-assembler-times "vfnmadd231ps" 4  } } */
/* { dg-final { scan-assembler-times "vfnmsub132ps" 4  } } */
/* { dg-final { scan-assembler-times "vfnmsub231ps" 4  } } */
/* { dg-final { scan-assembler-times "vfmadd132ss" 4  } } */
/* { dg-final { scan-assembler-times "vfmadd213ss" 4  } } */
/* { dg-final { scan-assembler-times "vfmsub132ss" 4  } } */
/* { dg-final { scan-assembler-times "vfmsub213ss" 4  } } */
/* { dg-final { scan-assembler-times "vfnmadd132ss" 4  } } */
/* { dg-final { scan-assembler-times "vfnmadd213ss" 4  } } */
/* { dg-final { scan-assembler-times "vfnmsub132ss" 4  } } */
/* { dg-final { scan-assembler-times "vfnmsub213ss" 4  } } */
