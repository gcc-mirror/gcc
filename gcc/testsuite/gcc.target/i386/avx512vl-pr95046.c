/* PR target/95046 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O3 -mavx512vl" } */

#include "pr95046-3.c"

/* { dg-final { scan-assembler "\tvfmadd\[123\]+ps" } } */
/* { dg-final { scan-assembler "\tvfmsub\[123\]+ps" } } */
/* { dg-final { scan-assembler "\tvfnmadd\[123\]+ps" } } */
/* { dg-final { scan-assembler "\tvfnmsub\[123\]+ps" } } */
