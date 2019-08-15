/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=256 --save-temps -mbig-endian" } */

#include "revh_1.c"

/* { dg-final { scan-assembler-not {\ttbl\t} } } */

/* { dg-final { scan-assembler-times {\trevh\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d} 2 } } */
/* { dg-final { scan-assembler-times {\trevh\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s} 2 } } */
