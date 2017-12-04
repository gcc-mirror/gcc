/* { dg-do compile } */
/* { dg-options "-mavx512f -mavx512er -mtune=knl -O2" } */

#include "pr82941-1.c"

/* { dg-final { scan-assembler-not "vzeroupper" } } */
