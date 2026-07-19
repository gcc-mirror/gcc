/* Signed vector division and modulus by negative powers of two.  */

typedef int v4si __attribute__ ((vector_size (16)));
typedef short v8hi __attribute__ ((vector_size (16)));
typedef long long v2di __attribute__ ((vector_size (16)));

#define SI_MIN (-__INT_MAX__ - 1)
#define DI_MIN (-__LONG_LONG_MAX__ - 1)

static const long long vals[]
  = { 0, 1, -1, 2, -2, 7, -7, 31, -31, 32, -32, 33, -33, 1000, -1000,
      0x7fff, -0x8000, __INT_MAX__, SI_MIN, __LONG_LONG_MAX__, DI_MIN,
      -1234567890123LL };
#define NVALS (sizeof (vals) / sizeof (vals[0]))

__attribute__ ((noipa)) static long long
sdiv (long long x, long long d)
{
  return x / d;
}

__attribute__ ((noipa)) static long long
smod (long long x, long long d)
{
  return x % d;
}

/* Divide and take the remainder of rotations of VALS by the constant
   vector {...}, and check every lane against the scalar result.  X / -1
   is undefined for the minimum value TMIN, so that lane gets 0 instead.  */
#define TEST(T, VT, N, TMIN, NAME, ...)					\
__attribute__ ((noipa)) static VT					\
q##NAME (VT x)								\
{									\
  return x / (VT) { __VA_ARGS__ };					\
}									\
__attribute__ ((noipa)) static VT					\
r##NAME (VT x)								\
{									\
  return x % (VT) { __VA_ARGS__ };					\
}									\
static void								\
check##NAME (void)							\
{									\
  static const T d[N] = { __VA_ARGS__ };				\
  for (unsigned j = 0; j < NVALS; j++)					\
    {									\
      VT x, q, r;							\
      for (int i = 0; i < N; i++)					\
	{								\
	  x[i] = (T) vals[(i + j) % NVALS];				\
	  if (d[i] == -1 && x[i] == TMIN)				\
	    x[i] = 0;							\
	}								\
      q = q##NAME (x);							\
      r = r##NAME (x);							\
      for (int i = 0; i < N; i++)					\
	if (q[i] != (T) sdiv (x[i], d[i])				\
	    || r[i] != (T) smod (x[i], d[i]))				\
	  __builtin_abort ();						\
    }									\
}

TEST (int, v4si, 4, SI_MIN, si1, -32, -32, -32, -32)
TEST (int, v4si, 4, SI_MIN, si2, -2, -4, -8, -16)
TEST (int, v4si, 4, SI_MIN, si3, -2, -2, -2, -(1 << 30))
TEST (int, v4si, 4, SI_MIN, si4, -4, 4, -4, 4)
TEST (int, v4si, 4, SI_MIN, si5, -4, -2, -1, -8)
TEST (int, v4si, 4, SI_MIN, si6, SI_MIN, SI_MIN, SI_MIN, SI_MIN)
TEST (short, v8hi, 8, -0x8000, hi1, -8, -8, -8, -8, -8, -8, -8, -8)
TEST (short, v8hi, 8, -0x8000, hi2, -2, -4, -8, -16, -32, -64, -128,
      -0x4000)
TEST (long long, v2di, 2, DI_MIN, di1, -8, -8)
TEST (long long, v2di, 2, DI_MIN, di2, -2, -(1LL << 62))
TEST (long long, v2di, 2, DI_MIN, di3, DI_MIN, DI_MIN)

int
main (void)
{
  checksi1 ();
  checksi2 ();
  checksi3 ();
  checksi4 ();
  checksi5 ();
  checksi6 ();
  checkhi1 ();
  checkhi2 ();
  checkdi1 ();
  checkdi2 ();
  checkdi3 ();
  return 0;
}
