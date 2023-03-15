/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2 -mno-sse4" } */

typedef unsigned int v2si __attribute__((vector_size(8)));
typedef float v2sf __attribute__((vector_size(8)));

v2si foo(v2si x,v2si y) { return (v2si){y[0],x[1]}; }
v2sf bar(v2sf x,v2sf y) { return (v2sf){y[0],x[1]}; }

/* { dg-final { scan-assembler-times "\tmovss\t" 2 } } */
/* { dg-final { scan-assembler-not "movaps" } } */
/* { dg-final { scan-assembler-not "shufps" } } */
/* { dg-final { scan-assembler-not "pblendw" } } */
