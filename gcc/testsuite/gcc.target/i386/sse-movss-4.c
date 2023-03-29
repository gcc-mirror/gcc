/* { dg-do compile } */
/* { dg-options "-O2 -msse -mno-sse4" } */

typedef unsigned int v4si __attribute__((vector_size(16)));
typedef float v4sf __attribute__((vector_size(16)));

v4si foo(v4si x,v4si y) { return (v4si){y[0],x[1],x[2],x[3]}; }
v4sf bar(v4sf x,v4sf y) { return (v4sf){y[0],x[1],x[2],x[3]}; }

/* { dg-final { scan-assembler-times "\tmovss\t" 2 } } */
/* { dg-final { scan-assembler-not "movaps" } } */
/* { dg-final { scan-assembler-not "shufps" } } */
/* { dg-final { scan-assembler-not "pblendw" } } */
