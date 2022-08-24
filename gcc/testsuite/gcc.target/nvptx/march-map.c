/* { dg-options "-march-map=sm_50" } */

#include "main.c"

/* { dg-final { scan-assembler-times "\\.target\tsm_35" 1 } } */
