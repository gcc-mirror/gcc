/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

float
quantum_real(float _Complex a)
{
  float *p = (float *) &a;
  return p[0];
}
float
quantum_imag(float _Complex a)
{
  float *p = (float *) &a;
  return p[1];
}
float
quantum_foo(float _Complex a)
{
  float *p = (float *) &a;
  return p[2];
}

/* { dg-final { scan-tree-dump-times "REALPART_EXPR" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "IMAGPART_EXPR" 1 "optimized" } } */
