/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details --save-temps" } */

#define TYPE uint8_t

#include "clastb_2.c"

/* { dg-final { scan-tree-dump "using a fully-masked loop." "vect" } } */
/* { dg-final { scan-assembler {\tclastb\tb[0-9]+, p[0-7], b[0-9]+, z[0-9]+\.b} } } */
