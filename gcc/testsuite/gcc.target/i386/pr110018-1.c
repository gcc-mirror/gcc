/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -mavx512vl -O2 -mavx512dq -fno-trapping-math" } */
/* { dg-final { scan-assembler-times {(?n)vcvttp[dsh]2[dqw]} 5 } } */
/* { dg-final { scan-assembler-times {(?n)vcvt[dqw]*2p[dsh]} 5 } } */

void
foo (double* __restrict a, signed char* b)
{
  a[0] = b[0];
  a[1] = b[1];
}

void
foo1 (float* __restrict a, signed char* b)
{
  a[0] = b[0];
  a[1] = b[1];
  a[2] = b[2];
  a[3] = b[3];
}

void
foo2 (_Float16* __restrict a, signed char* b)
{
  a[0] = b[0];
  a[1] = b[1];
  a[2] = b[2];
  a[3] = b[3];
  a[4] = b[4];
  a[5] = b[5];
  a[6] = b[6];
  a[7] = b[7];
}

void
foo3 (double* __restrict a, signed short* b)
{
  a[0] = b[0];
  a[1] = b[1];
}

void
foo4 (float* __restrict a, signed char* b)
{
  a[0] = b[0];
  a[1] = b[1];
  a[2] = b[2];
  a[3] = b[3];
}

void
foo5 (double* __restrict b, signed char* a)
{
  a[0] = b[0];
  a[1] = b[1];
}

void
foo6 (float* __restrict b, signed char* a)
{
  a[0] = b[0];
  a[1] = b[1];
  a[2] = b[2];
  a[3] = b[3];
}

void
foo7 (_Float16* __restrict b, signed char* a)
{
  a[0] = b[0];
  a[1] = b[1];
  a[2] = b[2];
  a[3] = b[3];
  a[4] = b[4];
  a[5] = b[5];
  a[6] = b[6];
  a[7] = b[7];
}

void
foo8 (double* __restrict b, signed short* a)
{
  a[0] = b[0];
  a[1] = b[1];
}

void
foo9 (float* __restrict b, signed char* a)
{
  a[0] = b[0];
  a[1] = b[1];
  a[2] = b[2];
  a[3] = b[3];
}
