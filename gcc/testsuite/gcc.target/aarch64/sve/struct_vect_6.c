/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#define TYPE double
#include "struct_vect_1.c"

/* { dg-final { scan-assembler {\tld2d\t{z[0-9]+.d - z[0-9]+.d}, p[0-7]/z, \[x[0-9]+\]\n} } } */
/* { dg-final { scan-assembler {\tld3d\t{z[0-9]+.d - z[0-9]+.d}, p[0-7]/z, \[x[0-9]+\]\n} } } */
/* { dg-final { scan-assembler {\tld4d\t{z[0-9]+.d - z[0-9]+.d}, p[0-7]/z, \[x[0-9]+\]\n} } } */
/* { dg-final { scan-assembler {\tst2d\t{z[0-9]+.d - z[0-9]+.d}, p[0-7], \[x[0-9]+\]\n} } } */
/* { dg-final { scan-assembler {\tst3d\t{z[0-9]+.d - z[0-9]+.d}, p[0-7], \[x[0-9]+\]\n} } } */
/* { dg-final { scan-assembler {\tst4d\t{z[0-9]+.d - z[0-9]+.d}, p[0-7], \[x[0-9]+\]\n} } } */
