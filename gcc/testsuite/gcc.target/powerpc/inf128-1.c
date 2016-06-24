/* { dg-do run { target { powerpc64*-*-* && vmx_hw } } } */
/* { dg-options "-mfloat128" } */

void abort ();

typedef unsigned long long int uint64_t;

typedef union
{
  __float128 value;

  struct
  {
#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    unsigned negative:1;
    unsigned exponent:15;
    unsigned quiet_nan:1;
    uint64_t mant_high:47;
    uint64_t mant_low:64;
#else
    uint64_t mant_low:64;
    uint64_t mant_high:47;
    unsigned quiet_nan:1;
    unsigned exponent:15;
    unsigned negative:1;
#endif
  } nan;

} ieee854_float128;

int
main (int argc, int *argv[])
{
  ieee854_float128 y;

  y.value = __builtin_infq ();

  if (y.nan.negative != 0
      || y.nan.exponent != 0x7fff
      || y.nan.quiet_nan != 0
      || y.nan.mant_high != 0
      || y.nan.mant_low != 0)
    abort ();

  y.value = __builtin_huge_valq ();

  if (y.nan.negative != 0
      || y.nan.exponent != 0x7fff
      || y.nan.quiet_nan != 0
      || y.nan.mant_high != 0
      || y.nan.mant_low != 0)
    abort ();

  return 0;
}
