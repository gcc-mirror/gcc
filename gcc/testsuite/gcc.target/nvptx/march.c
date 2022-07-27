/* { dg-options "-march=sm_30" } */

#include "main.c"

/* { dg-final { scan-assembler-times "\\.target\tsm_30" 1 } } */
