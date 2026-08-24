/* { dg-do run } */
/* { dg-options "-O2 -ffast-math -march=x86-64 -save-temps" } */

#define CHECK_CPU_SUPPORTS (__builtin_cpu_supports ("avx512vl") \
			    && __builtin_cpu_supports ("avx512bf16"))
#define ATTRIBUTE __attribute__ ((target("avx512vl,avx512bf16")))

#include "truncsfbf-check.h"

/* { dg-final { scan-assembler-times "\tvcvtneps2bf16" 1 } } */
