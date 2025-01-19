/* { dg-do compile } */
/* { dg-require-effective-target arm_cpu_cortex_m4_hard_ok } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_cpu_cortex_m4_hard } */

#include "fma.h"

/* { dg-final { scan-assembler-not "vfma\.f64\td\[0-9\]" } } */
/* { dg-final { scan-assembler-times "vfma\.f32\ts\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-not "vfms\.f64\td\[0-9\]" } } */
/* { dg-final { scan-assembler-times "vfms\.f32\ts\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-not "vfnma\.f64\td\[0-9\]" } } */
/* { dg-final { scan-assembler-times "vfnma\.f32\ts\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-not "vfnms\.f64\td\[0-9\]" } } */
/* { dg-final { scan-assembler-times "vfnms\.f32\ts\[0-9\]" 1 } } */
