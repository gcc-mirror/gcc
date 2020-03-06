/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake-avx512 -mprefer-vector-width=512" } */

#include "pr89229-2a.c"

/* { dg-final { scan-assembler-not "%zmm\[0-9\]+" } } */
