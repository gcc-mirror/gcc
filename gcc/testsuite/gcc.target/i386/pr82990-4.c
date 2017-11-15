/* { dg-do compile } */
/* { dg-options "-mavx512f -mno-avx512er -mno-vzeroupper -O2" } */

#include "pr82941-1.c"

/* { dg-final { scan-assembler-not "vzeroupper" } } */
