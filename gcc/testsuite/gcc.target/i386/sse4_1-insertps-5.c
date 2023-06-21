/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1" } */

typedef unsigned int v4si __attribute__((vector_size(16)));
typedef float v4sf __attribute__((vector_size(16)));

v4si foo_1(v4si x, v4si y) { return (v4si){x[0],y[3],x[2],x[3]}; }
v4si foo_2(v4si x, v4si y) { return (v4si){y[0],x[2],y[2],y[3]}; }
v4si foo_3(v4si x, v4si y) { return (v4si){x[3],y[1],y[2],y[3]}; }

v4sf bar_1(v4sf x, v4sf y) { return (v4sf){y[0],x[3],y[2],y[3]}; }
v4sf bar_2(v4sf x, v4sf y) { return (v4sf){x[0],y[2],x[2],x[3]}; }
v4sf bar_3(v4sf x, v4sf y) { return (v4sf){y[3],x[1],x[2],x[3]}; }

/* { dg-final { scan-assembler-times "\tv?insertps\t" 6 } } */
/* { dg-final { scan-assembler-not "pshufd" } } */
/* { dg-final { scan-assembler-not "pblendw" } } */
/* { dg-final { scan-assembler-not "shufps" } } */
/* { dg-final { scan-assembler-not "blendps" } } */
