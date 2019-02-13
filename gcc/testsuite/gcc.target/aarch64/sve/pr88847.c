/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-additional-options "-O0 -msve-vector-bits=256 -mbig-endian --save-temps" } */

typedef struct _b {
  __attribute__((__vector_size__(32))) int a[2];
} b;

b *c;

void
foo (void)
{
  char *p = '\0';
  b e = c[0];
}

/* { dg-final { scan-assembler {\tld1w\tz[0-9]+.s, p[0-9]+/z, \[x[0-9]+\]\n} } } */
/* { dg-final { scan-assembler {\tld1w\tz[0-9]+.s, p[0-9]+/z, \[x[0-9]+, #1, mul vl\]\n} } } */
/* { dg-final { scan-assembler {\tst1w\tz[0-9]+.s, p[0-9]+, \[(sp|x[0-9]+)\]\n} } } */
/* { dg-final { scan-assembler {\tst1w\tz[0-9]+.s, p[0-9]+, \[(sp|x[0-9]+), #1, mul vl\]\n} } } */

