
typedef float *__restrict__ pRF32;
typedef double *__restrict__ pRF64;

float maxv_f32 (pRF32 a)
{
  int i;
  float s = a[0];
  for (i=1;i<8;i++)
    s = (s > a[i] ? s :  a[i]);

  return s;
}

float minv_f32 (pRF32 a)
{
  int i;
  float s = a[0];
  for (i=1;i<16;i++)
    s = (s < a[i] ? s :  a[i]);

  return s;
}

double maxv_f64 (pRF64 a)
{
  int i;
  double s = a[0];
  for (i=1;i<8;i++)
    s = (s > a[i] ? s :  a[i]);

  return s;
}

double minv_f64 (pRF64 a)
{
  int i;
  double s = a[0];
  for (i=1;i<16;i++)
    s = (s < a[i] ? s :  a[i]);

  return s;
}
