
typedef float *__restrict__ pRF32;
typedef double *__restrict__ pRF64;


void max_F32 (pRF32 a, pRF32 b, pRF32 c)
{
  int i;
  for (i=0;i<16;i++)
    c[i] = (a[i] > b[i] ? a[i] : b[i]);
}

void min_F32 (pRF32 a, pRF32 b, pRF32 c)
{
  int i;
  for (i=0;i<16;i++)
    c[i] = (a[i] < b[i] ? a[i] : b[i]);
}

void max_F64 (pRF64 a, pRF64 b, pRF64 c)
{
  int i;
  for (i=0;i<16;i++)
    c[i] = (a[i] > b[i] ? a[i] : b[i]);
}

void min_F64 (pRF64 a, pRF64 b, pRF64 c)
{
  int i;
  for (i=0;i<16;i++)
    c[i] = (a[i] < b[i] ? a[i] : b[i]);
}
