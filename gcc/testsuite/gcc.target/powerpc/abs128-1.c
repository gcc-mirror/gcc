/* { dg-do run { target { powerpc64*-*-* && vsx_hw } } } */
/* { dg-options "-mfloat128 -mvsx" } */

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
  ieee854_float128 x, z;

  x.nan.negative = 1;
  x.nan.exponent = 0x22;
  x.nan.quiet_nan = 0;
  x.nan.mant_high = 0x1234;
  x.nan.mant_low = 0xabcdef;

  z.value = __builtin_fabsq (x.value);

  if (z.nan.negative != 0
      || z.nan.exponent != 0x22
      || z.nan.quiet_nan != 0
      || z.nan.mant_high != 0x1234
      || z.nan.mant_low != 0xabcdef)
    abort ();

  z.value = __builtin_fabsq (z.value);

  if (z.nan.negative != 0
      || z.nan.exponent != 0x22
      || z.nan.quiet_nan != 0
      || z.nan.mant_high != 0x1234
      || z.nan.mant_low != 0xabcdef)
    abort ();

  return 0;
}
