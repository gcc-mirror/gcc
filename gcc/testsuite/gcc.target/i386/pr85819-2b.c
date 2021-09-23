/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vl -mfpmath=sse" } */

#include "pr85819-2a.c"

/* { dg-final { scan-assembler "vcvtudq2ps" } } */
