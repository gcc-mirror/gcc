/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse4" } */

typedef unsigned long long v2di __attribute__((vector_size(16)));
typedef double v2df __attribute__((vector_size(16)));

v2di foo(v2di x,v2di y) { return (v2di){y[0],x[1]}; }
v2df bar(v2df x,v2df y) { return (v2df){y[0],x[1]}; }

/* { dg-final { scan-assembler-times "\tmovsd\t" 2 } } */
/* { dg-final { scan-assembler-not "shufpd" } } */
/* { dg-final { scan-assembler-not "movdqa" } } */
/* { dg-final { scan-assembler-not "pshufd" } } */
/* { dg-final { scan-assembler-not "punpckldq" } } */
/* { dg-final { scan-assembler-not "movq" } } */
