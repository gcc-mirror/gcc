/* { dg-do compile } */
/* { dg-options "-O3 -march=skylake -mfpmath=sse" } */

#include "pr88531-2a.c"

/* For ia32 we do not consider V2SFmode vectorization.  */
/* { dg-final { scan-assembler-times "vmulps" 1 { target { ! ia32 } } } } */
