/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details --save-temps" } */

#define TYPE int16_t

#include "clastb_2.c"

/* { dg-final { scan-tree-dump "operating on partial vectors." "vect" } } */
/* { dg-final { scan-assembler {\tclastb\th[0-9]+, p[0-7], h[0-9]+, z[0-9]+\.h} } } */
