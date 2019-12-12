/* PR target/91472 */
/* Reported by John Paul Adrian Glaubitz <glaubitz@physik.fu-berlin.de> */
/* { dg-require-effective-target double64plus } */

#if __SIZEOF_INT__ >= 4
typedef unsigned int gmp_uint_least32_t;
#else
typedef __UINT_LEAST32_TYPE__ gmp_uint_least32_t;
#endif

union ieee_double_extract
{
  struct
    {
      gmp_uint_least32_t sig:1;
      gmp_uint_least32_t exp:11;
      gmp_uint_least32_t manh:20;
      gmp_uint_least32_t manl:32;
    } s;
  double d;
};

double __attribute__((noipa))
tests_infinity_d (void)
{
  union ieee_double_extract x;
  x.s.exp = 2047;
  x.s.manl = 0;
  x.s.manh = 0;
  x.s.sig = 0;
  return x.d;
}

int
main (void)
{
  double x = tests_infinity_d ();
  if (x == 0.0)
    __builtin_abort ();
  return 0;
}
