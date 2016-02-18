/* { dg-do run } */
/* { dg-require-effective-target arm_v8_neon_hw } */
/* { dg-options "-O2 -ftree-vectorize -fno-inline -march=armv8-a -save-temps" } */
/* { dg-add-options arm_v8_neon } */

#include "fmaxmin.x"

/* { dg-final { scan-assembler-times "vmaxnm.f32\tq\[0-9\]+, q\[0-9\]+, q\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vminnm.f32\tq\[0-9\]+, q\[0-9\]+, q\[0-9\]+" 1 } } */

/* NOTE: There are no double precision vector versions of vmaxnm/vminnm.  */
/* { dg-final { scan-assembler-times "vmaxnm.f64\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vminnm.f64\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 1 } } */

