/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -dp" } */

#include "cond_fmaxnm_3.c"

/* { dg-final { scan-assembler-times {smaxv64sf3_exec} 3 } } */
/* { dg-final { scan-assembler-times {smaxv64df3_exec} 3 } } */

/* { dg-final { scan-assembler-not {\tv_writelane_b32\tv[0-9]+, vcc_..} } } */