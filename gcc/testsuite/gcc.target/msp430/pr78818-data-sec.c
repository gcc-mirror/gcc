/* { dg-do compile } */
/* { dg-options "-fdata-sections" } */

/* { dg-final { scan-assembler-not "\\.data" } } */
/* { dg-final { scan-assembler-not "\\.bss" } } */

#include "pr78818-real.c"
