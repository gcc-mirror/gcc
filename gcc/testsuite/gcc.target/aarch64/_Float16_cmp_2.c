/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.2-a+nofp16" } */

#include "_Float16_cmp_1.c"

/* { dg-final { scan-assembler-not {\tfcmp\th[0-9]+} } } */
/* { dg-final { scan-assembler-not {\tfcmpe\th[0-9]+} } } */
