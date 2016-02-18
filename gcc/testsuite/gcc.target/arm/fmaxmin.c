/* { dg-do run } */
/* { dg-require-effective-target arm_v8_neon_hw } */
/* { dg-options "-O2 -fno-inline -march=armv8-a -save-temps" } */
/* { dg-add-options arm_v8_neon } */

#include "fmaxmin.x"

/* { dg-final { scan-assembler-times "vmaxnm.f32\ts\[0-9\]+, s\[0-9\]+, s\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vminnm.f32\ts\[0-9\]+, s\[0-9\]+, s\[0-9\]+" 1 } } */

/* { dg-final { scan-assembler-times "vmaxnm.f64\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vminnm.f64\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 1 } } */

