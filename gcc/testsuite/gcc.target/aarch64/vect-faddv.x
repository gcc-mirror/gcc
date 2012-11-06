
typedef float *__restrict__ pRF32;
typedef double *__restrict__ pRF64;

float addv_f32 (pRF32 a)
{
  int i;
  float s = 0.0;
  for (i=0; i<16; i++)
    s += a[i];

  return s;
}

double addv_f64 (pRF64 a)
{
  int i;
  double  s = 0.0;
  for (i=0; i<16; i++)
    s += a[i];

  return s;
}
