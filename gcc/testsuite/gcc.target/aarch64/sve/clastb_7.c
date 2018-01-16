/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#define TYPE double
#include "clastb_6.c"

/* { dg-final { scan-assembler {\tclastb\td[0-9]+, p[0-7], d[0-9]+, z[0-9]+\.d} } } */
