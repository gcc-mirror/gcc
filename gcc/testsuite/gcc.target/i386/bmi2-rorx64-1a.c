/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mbmi2 -dp" } */

#include "bmi2-rorx64-1.c"

/* { dg-final { scan-assembler-times "bmi2_rorxdi3_1" 1 } } */
