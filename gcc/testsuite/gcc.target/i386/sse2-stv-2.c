/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

typedef long long v2di __attribute__((vector_size (16)));

long long m;

void foo(v2di x) { m = x[0]>>63; }

/* { dg-final { scan-assembler-not "ax" } } */
