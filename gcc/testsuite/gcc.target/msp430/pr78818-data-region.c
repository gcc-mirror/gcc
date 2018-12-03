/* { dg-do compile } */
/* { dg-options "-mdata-region=either" } */

/* { dg-final { scan-assembler-not "\\.either\\.data" } } */
/* { dg-final { scan-assembler-not "\\.either\\.bss" } } */

#include "pr78818-real.c"
