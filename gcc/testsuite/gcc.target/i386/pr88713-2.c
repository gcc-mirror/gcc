/* { dg-do compile } */
/* { dg-options "-Ofast -march=skylake-avx512 -mno-fma" } */

#include "pr88713-1.c"

/* { dg-final { scan-assembler "\tvfmadd\[123\]+ps" } } */
