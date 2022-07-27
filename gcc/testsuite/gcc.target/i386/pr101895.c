/* { dg-do compile } */
/* { dg-options "-O2 -march=cascadelake" } */

void foo(float * __restrict__ a, float b, float *c) {
  a[0] = c[0]*b + a[0];
  a[1] = c[2]*b + a[1];
  a[2] = c[1]*b + a[2];
  a[3] = c[3]*b + a[3];
}

/* { dg-final { scan-assembler "vfmadd" } } */
