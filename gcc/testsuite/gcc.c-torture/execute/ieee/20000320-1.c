#include <limits.h>
#if UINT_MAX != 4294967295u || (ULONG_LONG_MAX != 18446744073709551615ull && ULONG_MAX != 18446744073709551615ull)
int main(void) { exit (0); }
#else
#if ULONG_MAX != 18446744073709551615ull
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
    
  if (failed)
    abort ();
  else
    exit (0);
}
#endif
