extern double fma (double, double, double);
extern float fmaf (float, float, float);

float
vfma32 (float x, float y, float z)
{
  return fmaf (x, y, z);
}

float
vfms32 (float x, float y, float z)
{
  return fmaf (-x, y, z);
}

float
vfnms32 (float x, float y, float z)
{
  return fmaf (x, y, -z);
}

float
vfnma32 (float x, float y, float z)
{
  return fmaf (-x, y, -z);
}

double
vfma64 (double x, double y, double z)
{
  return fma (x, y, z);
}

double
vfms64 (double x, double y, double z)
{
  return fma (-x, y, z);
}

double
vfnms64 (double x, double y, double z)
{
  return fma (x, y, -z);
}

double
vfnma64 (double x, double y, double z)
{
  return fma (-x, y, -z);
}
