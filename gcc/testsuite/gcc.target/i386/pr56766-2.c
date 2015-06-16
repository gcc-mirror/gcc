/* PR target/56766 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model -mavx" } */

void test_v4sf (float * __restrict__ p, float * __restrict q)
{
  p[0] = p[0] - q[0];
  p[1] = p[1] + q[1];
  p[2] = p[2] - q[2];
  p[3] = p[3] + q[3];
}

void test_v8sf (float * __restrict__ p, float * __restrict q)
{
  p[0] = p[0] - q[0];
  p[1] = p[1] + q[1];
  p[2] = p[2] - q[2];
  p[3] = p[3] + q[3];
  p[4] = p[4] - q[4];
  p[5] = p[5] + q[5];
  p[6] = p[6] - q[6];
  p[7] = p[7] + q[7];
}

void test_v2df (double * __restrict__ p, double * __restrict q)
{
  p[0] = p[0] - q[0];
  p[1] = p[1] + q[1];
}

void test_v4df (double * __restrict__ p, double * __restrict q)
{
  p[0] = p[0] - q[0];
  p[1] = p[1] + q[1];
  p[2] = p[2] - q[2];
  p[3] = p[3] + q[3];
}

/* { dg-final { scan-assembler-times "vaddsubps" 2 } } */
/* { dg-final { scan-assembler-times "vaddsubpd" 2 } } */
