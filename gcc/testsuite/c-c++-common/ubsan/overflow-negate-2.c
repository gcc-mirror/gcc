/* { dg-do run } */
/* { dg-options "-fsanitize=signed-integer-overflow -Wno-unused-variable" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } { "" } } */

#define SCHAR_MIN (-__SCHAR_MAX__ - 1)
#define SHRT_MIN (-__SHRT_MAX__ - 1)
#define INT_MIN (-__INT_MAX__ - 1)
#define LONG_MIN (-__LONG_MAX__ - 1L)
#define LLONG_MIN (-__LONG_LONG_MAX__ - 1LL)

#define CHECK(A, B) ({ if ((A) != (B)) __builtin_abort (); })

int
main (void)
{
  volatile signed char c = -SCHAR_MIN;
  CHECK (c, -128);

  volatile short s = -SHRT_MIN;
  CHECK (s, -32768);

  volatile int i = INT_MIN;
  i = -(unsigned) i;
  CHECK (i, -0x80000000);

  volatile long int li = LONG_MIN;
  li = -(unsigned long) li;
#if __LONG_MAX__ == 2147483647L
  CHECK (li, -0x80000000L);
#elif __LONG_MAX__ == 9223372036854775807L
  CHECK (li, -0x8000000000000000L);
#endif

  volatile long long lli = LLONG_MIN;
  lli = -(unsigned long long) lli;
  CHECK (lli, -0x8000000000000000L);

  return 0;
}
