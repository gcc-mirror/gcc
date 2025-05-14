/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8a_hard_ok } */
/* { dg-options "-O2 -fno-inline" } */
/* { dg-add-options arm_arch_v8a_hard } */

#include "fmaxmin.x"

/* { dg-final { scan-assembler-times "vmaxnm.f32\ts\[0-9\]+, s\[0-9\]+, s\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vminnm.f32\ts\[0-9\]+, s\[0-9\]+, s\[0-9\]+" 1 } } */

/* { dg-final { scan-assembler-times "vmaxnm.f64\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "vminnm.f64\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" 1 } } */
