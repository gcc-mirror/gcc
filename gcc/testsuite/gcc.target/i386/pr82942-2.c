/* { dg-do compile } */
/* { dg-options "-mavx512f -mavx512er -mtune=knl -O2" } */
/* { dg-warning "AVX512ER support will be removed in GCC 15" "" { target *-*-* } 0 } */
/* { dg-warning "'-mtune=knl' support will be removed in GCC 15" "" { target *-*-* } 0 } */

#include "pr82941-1.c"

/* { dg-final { scan-assembler-not "vzeroupper" } } */
