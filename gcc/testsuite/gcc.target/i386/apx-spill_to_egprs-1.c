/* { dg-do compile  { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake-avx512 -mapxf -DDTYPE32" } */

#include "spill_to_mask-1.c"

/* Make sure that no local variables are stored on the stack. */
/* { dg-final { scan-assembler-not "\\(%rsp\\)" } } */

/* { dg-final { scan-assembler-not "knot" } } */
/* { dg-final { scan-assembler-not "kxor" } } */
/* { dg-final { scan-assembler-not "kor" } } */
/* { dg-final { scan-assembler-not "kandn" } } */
