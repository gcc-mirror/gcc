#if __INT_MAX__ != 2147483647 || (__LONG_LONG_MAX__ != 9223372036854775807ll && __LONG_MAX__ != 9223372036854775807ll)
int main(void) { exit (0); }
#else
#if __LONG_MAX__ != 9223372036854775807ll
typedef unsigned long long ull;
#else
typedef unsigned long ull;
#endif
typedef unsigned ul;

union fl {
  float	f;
  ul l;
} uf;
union dl {
  double d;
  ull ll;
} ud;

int failed = 0;

void c(ull d, ul f)
{
  ud.ll = d;
  uf.f = (float) ud.d;
  if (uf.l != f)
    {
      failed++;
    }
}

int main()
{
  if (sizeof (float) != sizeof (ul)
      || sizeof (double) != sizeof (ull))
    exit (0);
  
#if (defined __arm__ || defined __thumb__) && ! (defined __ARMEB__ || defined __VFP_FP__)
  /* The ARM always stores FP numbers in big-wordian format,
     even when running in little-byteian mode.  */
  c(0x0000000036900000ULL, 0x00000000U);
  c(0x0000000136900000ULL, 0x00000001U);
  c(0xffffffff369fffffULL, 0x00000001U);
  c(0x0000000036A00000ULL, 0x00000001U);
  c(0xffffffff36A7ffffULL, 0x00000001U);
  c(0x0000000036A80000ULL, 0x00000002U);
  c(0xffffffff36AfffffULL, 0x00000002U);
  c(0x0000000036b00000ULL, 0x00000002U);
  c(0x0000000136b00000ULL, 0x00000002U);
  
  c(0xdfffffff380fffffULL, 0x007fffffU);
  c(0xe0000000380fffffULL, 0x00800000U);
  c(0xe0000001380fffffULL, 0x00800000U);
  c(0xffffffff380fffffULL, 0x00800000U);
  c(0x0000000038100000ULL, 0x00800000U);
  c(0x0000000138100000ULL, 0x00800000U);
  c(0x1000000038100000ULL, 0x00800000U);
  c(0x1000000138100000ULL, 0x00800001U);
  c(0x2fffffff38100000ULL, 0x00800001U);
  c(0x3000000038100000ULL, 0x00800002U);
  c(0x5000000038100000ULL, 0x00800002U);
  c(0x5000000138100000ULL, 0x00800003U);
#else
  c(0x3690000000000000ULL, 0x00000000U);
  c(0x3690000000000001ULL, 0x00000001U);
  c(0x369fffffffffffffULL, 0x00000001U);
  c(0x36A0000000000000ULL, 0x00000001U);
  c(0x36A7ffffffffffffULL, 0x00000001U);
  c(0x36A8000000000000ULL, 0x00000002U);
  c(0x36AfffffffffffffULL, 0x00000002U);
  c(0x36b0000000000000ULL, 0x00000002U);
  c(0x36b0000000000001ULL, 0x00000002U);
  
  c(0x380fffffdfffffffULL, 0x007fffffU);
  c(0x380fffffe0000000ULL, 0x00800000U);
  c(0x380fffffe0000001ULL, 0x00800000U);
  c(0x380fffffffffffffULL, 0x00800000U);
  c(0x3810000000000000ULL, 0x00800000U);
  c(0x3810000000000001ULL, 0x00800000U);
  c(0x3810000010000000ULL, 0x00800000U);
  c(0x3810000010000001ULL, 0x00800001U);
  c(0x381000002fffffffULL, 0x00800001U);
  c(0x3810000030000000ULL, 0x00800002U);
  c(0x3810000050000000ULL, 0x00800002U);
  c(0x3810000050000001ULL, 0x00800003U);
#endif

  if (failed)
    abort ();
  else
    exit (0);
}
#endif
