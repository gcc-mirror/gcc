#if defined(PPC64LE)
typedef __float128 longreal_t;
#else
typedef long double longreal_t;
#endif

longreal_t
cstub_identicall (longreal_t x)
{
  return x;
}

double
cstub_identical (double x)
{
  return x;
}


float
cstub_identicalf (float x)
{
  return x;
}
