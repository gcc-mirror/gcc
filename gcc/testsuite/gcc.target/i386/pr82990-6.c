/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512 -mtune=knl" } */

#include "pr82941-1.c"

/* { dg-final { scan-assembler-not "vzeroupper" } } */
