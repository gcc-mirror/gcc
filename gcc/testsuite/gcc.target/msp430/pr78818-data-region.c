/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=msp430" } { "" } } */
/* { dg-options "-mlarge -mdata-region=either" } */

/* { dg-final { scan-assembler-not "\\.either\\.data" } } */
/* { dg-final { scan-assembler-not "\\.either\\.bss" } } */

#include "pr78818-real.c"
