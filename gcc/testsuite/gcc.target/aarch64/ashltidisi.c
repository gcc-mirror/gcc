/* { dg-do run } */
/* { dg-options "-O2 -save-temps" } */

extern void abort (void);

#define GEN_TEST_CASE(x, y, z)\
__uint128_t __attribute__ ((noinline))\
ushift_##x##_##z (unsigned y data)\
{\
  return (__uint128_t) data << x;\
}\
__int128_t __attribute__ ((noinline)) \
shift_##x##_##z (y data) \
{\
  return (__int128_t) data << x;\
}

GEN_TEST_CASE (53, int, i)
GEN_TEST_CASE (3, long long, ll)
GEN_TEST_CASE (13, long long, ll)
GEN_TEST_CASE (53, long long, ll)

int
main (int argc, char **argv)
{

#define SHIFT_CHECK(x, y, z, p) \
	if (ushift_##y##_##p (x)\
	    != ((__uint128_t) (unsigned z) x << y)) \
	  abort ();\
	if (shift_##y##_##p (x)\
	    != ((__uint128_t) (signed z) x << y)) \
	  abort ();

  SHIFT_CHECK (0x12345678, 53, int, i)
  SHIFT_CHECK (0xcafecafe, 53, int, i)

  SHIFT_CHECK (0x1234567890abcdefLL, 3, long long, ll)
  SHIFT_CHECK (0x1234567890abcdefLL, 13, long long, ll)
  SHIFT_CHECK (0x1234567890abcdefLL, 53, long long, ll)
  SHIFT_CHECK (0xcafecafedeaddeadLL, 3, long long, ll)
  SHIFT_CHECK (0xcafecafedeaddeadLL, 13, long long, ll)
  SHIFT_CHECK (0xcafecafedeaddeadLL, 53, long long, ll)

  return 0;
}

/* { dg-final { scan-assembler-times "asr" 3 } } */
/* { dg-final { scan-assembler-not "extr\t" } } */
