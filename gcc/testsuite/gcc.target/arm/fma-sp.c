/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=cortex-m4 -mfpu=fpv4-sp-d16 -mthumb" } */

#include "fma.h"

/* { dg-final { scan-assembler-not "vfma\.f64\td\[0-9\]" } } */
/* { dg-final { scan-assembler-times "vfma\.f32\ts\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-not "vfms\.f64\td\[0-9\]" } } */
/* { dg-final { scan-assembler-times "vfms\.f32\ts\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-not "vfnma\.f64\td\[0-9\]" } } */
/* { dg-final { scan-assembler-times "vfnma\.f32\ts\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-not "vfnms\.f64\td\[0-9\]" } } */
/* { dg-final { scan-assembler-times "vfnms\.f32\ts\[0-9\]" 1 } } */
