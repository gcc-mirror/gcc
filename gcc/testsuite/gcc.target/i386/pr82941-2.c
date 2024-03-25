/* { dg-do compile } */
/* { dg-options "-O2 -march=knl" } */
/* { dg-warning "'-march=knl' support will be removed in GCC 15" "" { target *-*-* } 0 } */

#include "pr82941-1.c"

/* { dg-final { scan-assembler-not "vzeroupper" } } */
