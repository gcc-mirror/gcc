/* { dg-do assemble } */
/* { dg-options "-O2 --save-temps" } */

#pragma GCC target "+nosimd"

#include "ins_bitfield_5.c"

/* { dg-final { scan-assembler-not {\tins\t} } } */
