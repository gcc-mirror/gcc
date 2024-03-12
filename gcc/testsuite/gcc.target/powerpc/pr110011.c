/* { dg-do run } */
/* { dg-require-effective-target float128_runtime } */
/* Force long double to be with IBM format here, to verify
   _Float128 constant still uses its own format (IEEE) for
   encoding rather than IBM format.  */
/* { dg-options "-mfp-in-toc -mabi=ibmlongdouble" } */
/* { dg-add-options float128 } */

#define MPFR_FLOAT128_MAX 0x1.ffffffffffffffffffffffffffffp+16383f128

__attribute__ ((noipa))
_Float128 f128_max ()
{
  return MPFR_FLOAT128_MAX;
}

typedef union
{
  int w[4];
  _Float128 f128;
} U;

int main ()
{

  U umax;
  umax.f128 = f128_max ();
  /* ieee float128 max:
     7ffeffff ffffffff ffffffff ffffffff.  */
  if (umax.w[1] != 0xffffffff || umax.w[2] != 0xffffffff)
    __builtin_abort ();
#ifdef __LITTLE_ENDIAN__
  if (umax.w[0] != 0xffffffff || umax.w[3] != 0x7ffeffff)
    __builtin_abort ();
#else
  if (umax.w[3] != 0xffffffff || umax.w[0] != 0x7ffeffff)
    __builtin_abort ();
#endif

  return 0;
}

