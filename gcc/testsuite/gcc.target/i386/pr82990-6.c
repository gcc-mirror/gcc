/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512 -mtune=knl" } */
/* { dg-warning "'-mtune=knl' support will be removed in GCC 15" "" { target *-*-* } 0 } */

#include "pr82941-1.c"

/* { dg-final { scan-assembler-not "vzeroupper" } } */
