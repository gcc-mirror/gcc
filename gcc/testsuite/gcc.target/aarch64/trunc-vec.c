/* { dg-do compile } */
/* { dg-options "-O2" } */

/* { dg-final { scan-assembler-times {fcvtn\tv[0-9]+.2s, v[0-9]+.2d} 1 } } */
void
f (double *__restrict a, float *__restrict b)
{
  b[0] = a[0];
  b[1] = a[1];
}

/* { dg-final { scan-assembler-times {fcvtn\tv[0-9]+.4h, v[0-9]+.4s} 1 } } */
void
f1 (float *__restrict a, _Float16 *__restrict b)
{

  b[0] = a[0];
  b[1] = a[1];
  b[2] = a[2];
  b[3] = a[3];
}
