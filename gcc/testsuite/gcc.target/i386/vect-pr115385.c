/* { dg-do compile } */
/* { dg-options "-O3 -msse4.1 -mno-avx -fdump-tree-vect-details" } */

void __attribute__((noipa)) foo(unsigned char * __restrict x,
                                unsigned char *y, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[16*i+0] = y[3*i+0];
      x[16*i+1] = y[3*i+1];
      x[16*i+2] = y[3*i+2];
      x[16*i+3] = y[3*i+0];
      x[16*i+4] = y[3*i+1];
      x[16*i+5] = y[3*i+2];
      x[16*i+6] = y[3*i+0];
      x[16*i+7] = y[3*i+1];
      x[16*i+8] = y[3*i+2];
      x[16*i+9] = y[3*i+0];
      x[16*i+10] = y[3*i+1];
      x[16*i+11] = y[3*i+2];
      x[16*i+12] = y[3*i+0];
      x[16*i+13] = y[3*i+1];
      x[16*i+14] = y[3*i+2];
      x[16*i+15] = y[3*i+0];
    }
}

void __attribute__((noipa)) bar(unsigned char * __restrict x,
                                unsigned char *y, int n)
{
  for (int i = 0; i < n; ++i)
    {
      x[16*i+0] = y[5*i+0];
      x[16*i+1] = y[5*i+1];
      x[16*i+2] = y[5*i+2];
      x[16*i+3] = y[5*i+3];
      x[16*i+4] = y[5*i+4];
      x[16*i+5] = y[5*i+0];
      x[16*i+6] = y[5*i+1];
      x[16*i+7] = y[5*i+2];
      x[16*i+8] = y[5*i+3];
      x[16*i+9] = y[5*i+4];
      x[16*i+10] = y[5*i+0];
      x[16*i+11] = y[5*i+1];
      x[16*i+12] = y[5*i+2];
      x[16*i+13] = y[5*i+3];
      x[16*i+14] = y[5*i+4];
      x[16*i+15] = y[5*i+0];
    }
}

/* { dg-final { scan-tree-dump "Data access with gaps requires scalar epilogue loop" "vect"} } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect"} } */
