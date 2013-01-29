/* { dg-do compile } */
/* { dg-skip-if "avoid conflicts with multilib options" { ! arm_thumb2_ok } { "-mthumb" } { "" } } */
/* { dg-skip-if "avoid conflicts with multilib options" { *-*-* } { "-mcpu=*" } { "-mcpu=cortex-a15" } } */
/* { dg-skip-if "avoid conflicts with multilib options" { *-*-* } { "-mfpu=*" } { "-mfpu=vfpv4" } } */
/* { dg-skip-if "avoid conflicts with multilib options" { *-*-* } { "-mfloat-abi=*" } { "-mfloat-abi=hard" } } */
/* { dg-options "-O2 -mcpu=cortex-a15 -mfpu=vfpv4 -mfloat-abi=hard" } */

#include "fma.h"

/* { dg-final { scan-assembler-times "vfma\.f64\td\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vfma\.f32\ts\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vfms\.f64\td\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vfms\.f32\ts\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vfnma\.f64\td\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vfnma\.f32\ts\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vfnms\.f64\td\[0-9\]" 1 } } */
/* { dg-final { scan-assembler-times "vfnms\.f32\ts\[0-9\]" 1 } } */
