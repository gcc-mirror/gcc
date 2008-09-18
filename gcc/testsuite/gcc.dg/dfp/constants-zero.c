/* { dg-options "-std=gnu99 -O0" } */

/* Decimal float values can have significant trailing zeroes.  This is
   true for zero values as well.  Check that various representations of
   zero are handled correctly when specified as literal constants.  */

extern void abort (void);

int big_endian;

typedef union U32 {
  unsigned int i;
  _Decimal32 d;
  unsigned char b[4];
} u32_t;

typedef union U64 {
  unsigned long long i;
  _Decimal64 d;
} u64_t;

typedef union U128 {
  unsigned long long i[2];
  _Decimal128 d;
} u128_t;

int
compare32 (_Decimal32 d, unsigned int i)
{
  u32_t u;

  u.d = d;
  return (u.i == i);
}

int
compare64 (_Decimal64 d, unsigned long long i)
{
  u64_t u;

  u.d = d;
  return (u.i == i);
}

int
compare128 (_Decimal64 d, unsigned long long i, unsigned long long j)
{
  u128_t u;

  u.d = d;
  if (big_endian)
    return (u.i[0] == i && u.i[1] == j);
  else
    return (u.i[1] == i && u.i[0] == j);
}

void
dpd_tests (void)
{
  if (! compare32 (0.DF, 0x22500000U))
    abort ();
  if (! compare32 (-0.DF, 0xa2500000U))
    abort ();
  if (! compare32 (0.E-4DF, 0x22100000U))
    abort ();
  if (! compare32 (0.E-7DF, 0x21e00000U))
    abort ();
  if (! compare32 (0.E+3DF, 0x22800000U))
    abort ();

  if (! compare64 (0.DD, 0x2238000000000000ULL))
    abort ();
  if (! compare64 (-0.DD, 0xa238000000000000ULL))
    abort ();
  if (! compare64 (0.E-6DD, 0x2220000000000000ULL))
    abort ();
  if (! compare64 (0.E-7DD, 0x221c000000000000ULL))
    abort ();
  if (! compare64 (0.E+2DD, 0x2240000000000000ULL))
    abort ();

  if (! compare128 (0.DL, 0x2208000000000000ULL, 0x0000000000000000ULL))
    abort ();
  if (! compare128 (-0.DL, 0xa208000000000000ULL, 0x0000000000000000ULL))
    abort ();
  if (! compare128 (0.E-3DL, 0x2207400000000000ULL, 0x0000000000000000ULL))
    abort ();
  if (! compare128 (0.E-8DL, 0x2206000000000000ULL, 0x0000000000000000ULL))
    abort ();
  if (! compare128 (0.E+2DL, 0x2208800000000000ULL, 0x0000000000000000ULL))
    abort ();
}

void
bid_tests (void)
{
  if (! compare32 (0.DF, 0x32800000U))
    abort ();
  if (! compare32 (-0.DF, 0xb2800000U))
    abort ();
  if (! compare32 (0.E-4DF, 0x30800000U))
    abort ();
  if (! compare32 (0.E-7DF, 0x2f000000U))
    abort ();
  if (! compare32 (0.E+3DF, 0x34000000U))
    abort ();

  if (! compare64 (0.DD, 0x31c0000000000000ULL))
    abort ();
  if (! compare64 (-0.DD, 0xb1c0000000000000ULL))
    abort ();
  if (! compare64 (0.E-6DD, 0x3100000000000000ULL))
    abort ();
  if (! compare64 (0.E-7DD, 0x30e0000000000000ULL))
    abort ();
  if (! compare64 (0.E+2DD, 0x3200000000000000ULL))
    abort ();

  if (! compare128 (0.DL, 0x3040000000000000ULL, 0x0000000000000000ULL))
    abort ();
  if (! compare128 (-0.DL, 0xb040000000000000ULL, 0x0000000000000000ULL))
    abort ();
  if (! compare128 (0.E-3DL, 0x303a000000000000ULL, 0x0000000000000000ULL))
    abort ();
  if (! compare128 (0.E-8DL, 0x3030000000000000ULL, 0x0000000000000000ULL))
    abort ();
  if (! compare128 (0.E+2DL, 0x3044000000000000ULL, 0x0000000000000000ULL))
    abort ();
}

int
main ()
{
  u32_t u32;
  
  /* These sizes are probably always true for targets that support decimal
     float types, but check anyway.  Abort so we can fix the test.  */
  if ((sizeof (_Decimal64) != sizeof (long long))
      || (sizeof (_Decimal128) != 2 * sizeof (long long))
      || (sizeof (_Decimal32) != sizeof (_Decimal32)))
    abort ();

  u32.d = 1.DF;

  if (u32.i == 0x22500001)
    {
      big_endian = (u32.b[0] == 0x22);
      dpd_tests ();
    }
  else if (u32.i == 0x32800001)
    {
       big_endian = (u32.b[0] == 0x32);
       bid_tests ();
    }
  else
    abort ();		/* unknown format; test problem  */

  return 0;
}
