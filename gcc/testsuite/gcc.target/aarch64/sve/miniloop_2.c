/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps -msve-vector-bits=256" } */

#include "miniloop_1.c"

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, } 8 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, } 8 } } */
