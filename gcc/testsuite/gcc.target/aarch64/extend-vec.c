/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {fcvtl\tv[0-9]+.2d, v[0-9]+.2s} 1 } } */
void
f (float *__restrict a, double *__restrict b)
{
  b[0] = a[0];
  b[1] = a[1];
}

/* { dg-final { scan-assembler-times {fcvtl\tv[0-9]+.4s, v[0-9]+.4h} 1 } } */
void
f1 (_Float16 *__restrict a, float *__restrict b)
{

  b[0] = a[0];
  b[1] = a[1];
  b[2] = a[2];
  b[3] = a[3];
}
