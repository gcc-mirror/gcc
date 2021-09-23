/* { dg-do compile  { target { ! ia32 } } } */
/* { dg-options "-O2 -march=skylake-avx512 -DDTYPE64" } */

#include "spill_to_mask-1.c"

/* { dg-final { scan-assembler-not "knot" } } */
/* { dg-final { scan-assembler-not "kxor" } } */
/* { dg-final { scan-assembler-not "kor" } } */
/* { dg-final { scan-assembler-not "kandn" } } */
