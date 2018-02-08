/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#define TYPE uint64_t

#include "clastb_2.c"

/* { dg-final { scan-assembler {\tclastb\tx[0-9]+, p[0-7], x[0-9]+, z[0-9]+\.d} } } */
