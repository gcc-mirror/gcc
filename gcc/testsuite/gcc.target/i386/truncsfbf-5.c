/* { dg-do run } */
/* { dg-options "-O2 -ffast-math -march=x86-64 -save-temps" } */

#define CHECK_CPU_SUPPORTS __builtin_cpu_supports ("avxneconvert")
#define ATTRIBUTE __attribute__ ((target("avxneconvert")))

#include "truncsfbf-check.h"

/* { dg-final { scan-assembler-times "\t{vex} vcvtneps2bf16" 1 } } */
