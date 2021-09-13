/* { dg-do compile } */
/* { dg-options "-O3 -march=skylake-avx512 -mfpmath=sse" } */

#include "pr88531-1a.c"

/* { dg-final { scan-assembler-times "vgatherdpd" 4 { target { ! lp64 } } } } */
/* { dg-final { scan-assembler-times "vgatherqpd" 4 { target lp64 } } } */
/* { dg-final { scan-assembler-times "vmulpd" 4 } } */
