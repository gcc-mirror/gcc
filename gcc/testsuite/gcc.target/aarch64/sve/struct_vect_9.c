/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#define TYPE uint32_t
#define ITYPE int32_t
#include "struct_vect_7.c"

/* { dg-final { scan-assembler {\tld2w\t{z[0-9]+.s - z[0-9]+.s}, p[0-7]/z, \[x[0-9]+\]\n} } } */
/* { dg-final { scan-assembler {\tld3w\t{z[0-9]+.s - z[0-9]+.s}, p[0-7]/z, \[x[0-9]+\]\n} } } */
/* { dg-final { scan-assembler {\tld4w\t{z[0-9]+.s - z[0-9]+.s}, p[0-7]/z, \[x[0-9]+\]\n} } } */
/* { dg-final { scan-assembler {\tst2w\t{z[0-9]+.s - z[0-9]+.s}, p[0-7], \[x[0-9]+\]\n} } } */
/* { dg-final { scan-assembler {\tst3w\t{z[0-9]+.s - z[0-9]+.s}, p[0-7], \[x[0-9]+\]\n} } } */
/* { dg-final { scan-assembler {\tst4w\t{z[0-9]+.s - z[0-9]+.s}, p[0-7], \[x[0-9]+\]\n} } } */
