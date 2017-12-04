/* { dg-do compile } */
/* { dg-options "-O2 -march=skylake-avx512 -mtune=generic -mtune-ctrl=^emit_vzeroupper" } */

#include "pr82941-1.c"

/* { dg-final { scan-assembler-not "vzeroupper" } } */
