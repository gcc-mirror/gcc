// Test to exercise that the type-specific integer arithmetic built-ins
// with overflow checking can be used in C++ 14 constant expressions.
// -Woverflow is disabled to prevent (bogus?) G++ warnings.
// { dg-do compile { target c++14 } }
// { dg-additional-options "-Wno-overflow" }

#define SCHAR_MAX    __SCHAR_MAX__
#define SHRT_MAX     __SHRT_MAX__
#define INT_MAX	     __INT_MAX__
#define LONG_MAX     __LONG_MAX__
#define LLONG_MAX    __LONG_LONG_MAX__
	
#define SCHAR_MIN    (-__SCHAR_MAX__ - 1)
#define SHRT_MIN     (-__SHRT_MAX__ - 1)
#define INT_MIN	     (-__INT_MAX__ - 1)
#define LONG_MIN     (-__LONG_MAX__ - 1)
#define LLONG_MIN    (-__LONG_LONG_MAX__ - 1)
	
#define UCHAR_MAX    (SCHAR_MAX * 2U + 1)
#define USHRT_MAX    (SHRT_MAX * 2U + 1)
#define UINT_MAX     (INT_MAX * 2U + 1)
#define ULONG_MAX    (LONG_MAX * 2LU + 1)
#define ULLONG_MAX   (LLONG_MAX * 2LLU + 1)
	
#define USCHAR_MIN   (-__USCHAR_MAX__ - 1)
#define USHRT_MIN    (-__USHRT_MAX__ - 1)
#define UINT_MIN     (-__UINT_MAX__ - 1)
#define ULONG_MIN    (-__ULONG_MAX__ - 1)
#define ULLONG_MIN   (-__ULONG_LONG_MAX__ - 1)

// Helper macros.
#define sadd(x, y) ((x) + (y))
#define saddl(x, y) ((x) + (y))
#define saddll(x, y) ((x) + (y))
#define uadd(x, y) ((x) + (y))
#define uaddl(x, y) ((x) + (y))
#define uaddll(x, y) ((x) + (y))
#define ssub(x, y) ((x) - (y))
#define ssubl(x, y) ((x) - (y))
#define ssubll(x, y) ((x) - (y))
#define usub(x, y) ((x) - (y))
#define usubl(x, y) ((x) - (y))
#define usubll(x, y) ((x) - (y))
#define smul(x, y) ((x) * (y))
#define smull(x, y) ((x) * (y))
#define smulll(x, y) ((x) * (y))
#define umul(x, y) ((x) * (y))
#define umull(x, y) ((x) * (y))
#define umulll(x, y) ((x) * (y))

// Result object.
template <class T>
struct Res
{
  constexpr Res (T a, bool v): z (a), v (v) { }
  T z; bool v;
};

template <class T>
constexpr bool operator== (Res<T> a, Res<T> b)
{
  return a.z == b.z && a.v == b.v;
}

#define StaticAssert(expr) static_assert ((expr), #expr)

#define CONCAT(a, b)   a ## b
#define CAT(a, b)      CONCAT (a, b)

// Helper to determine the type of the result of the arithmetic
// as specified by the built-ins.
template <class T> struct ResType { typedef T type; };
template <>        struct ResType<signed char> { typedef int type; };
template <>        struct ResType<unsigned char> { typedef unsigned type; };
template <>        struct ResType<signed short> { typedef int type; };
template <>        struct ResType<unsigned short> { typedef unsigned type; };

// Macro to define a single test case verifying that integer overflow
// is detected when expected, and when not, that the result matches
// the result computed using ordinary arithmetic.  The result cannot
// be tested in the presence of overflow since it's not a core
// constant expression.
#define TEST(op, T, x, y, vflow)					\
  constexpr Res<T> CAT (op, __LINE__)(T a, T b)				\
  {									\
    ResType<T>::type c = 0;						\
    bool v = __builtin_ ## op ## _overflow (a, b, &c);			\
    return Res<T>(c, v);						\
  }									\
  StaticAssert (vflow ? CAT (op, __LINE__)(x, y).v			\
		: CAT (op, __LINE__)(x, y) == Res<T>(op (x, y), vflow))

/* Signed int addition.  */
TEST (sadd,   signed char,    0,         0,         0);
TEST (sadd,   signed char,    0,         SCHAR_MAX, 0);
TEST (sadd,   signed char,    1,         SCHAR_MAX, 0);
TEST (sadd,   signed char,    SCHAR_MAX, SCHAR_MAX, 0);
TEST (sadd,   signed char,    0,         SCHAR_MIN, 0);
TEST (sadd,   signed char,   -1,         SCHAR_MIN, 0);

TEST (sadd,   short,          0,         0,         0);
TEST (sadd,   short,          0,         SHRT_MAX,  0);
TEST (sadd,   short,          1,         SHRT_MAX,  0);
TEST (sadd,   short,          SHRT_MAX,  SHRT_MAX,  0);
TEST (sadd,   short,          0,         SHRT_MIN,  0);
TEST (sadd,   short,         -1,         SHRT_MIN,  0);
TEST (sadd,   short,          SHRT_MIN,  SHRT_MIN,  0);

TEST (sadd,   int,            0,         0,         0);
TEST (sadd,   int,            0,         INT_MAX,   0);
TEST (sadd,   int,            1,         INT_MAX,   1);
TEST (sadd,   int,            INT_MAX,   INT_MAX,   1);
TEST (sadd,   int,            0,         INT_MIN,   0);
TEST (sadd,   int,           -1,         INT_MIN,   1);
TEST (sadd,   int,            INT_MIN,   INT_MIN,   1);

/* Signed long addition.  */
TEST (saddl,  long,           0L,        0L,        0);
TEST (saddl,  long,           0L,        LONG_MAX,  0);
TEST (saddl,  long,           1L,        LONG_MAX,  1);
TEST (saddl,  long,           LONG_MAX,  LONG_MAX,  1);
TEST (saddl,  long,           0L,        LONG_MIN,  0);
TEST (saddl,  long,          -1L,        LONG_MIN,  1);
TEST (saddl,  long,           LONG_MIN,  LONG_MIN,  1);

TEST (saddll, long long,      0LL,       0LL,        0);
TEST (saddll, long long,      0LL,       LLONG_MAX,  0);
TEST (saddll, long long,      1LL,       LLONG_MAX,  1);
TEST (saddll, long long,      LLONG_MAX, LLONG_MAX,  1);
TEST (saddll, long long,      0LL,       LLONG_MIN,  0);
TEST (saddll, long long,     -1LL,       LLONG_MIN,  1);
TEST (saddll, long long,      LLONG_MIN, LLONG_MIN,  1);

/* Unsigned int addition.  */
TEST (uadd,   unsigned char,  0U,        0U,         0);
TEST (uadd,   unsigned char,  0U,        UCHAR_MAX, 0);
TEST (uadd,   unsigned char,  1U,        UCHAR_MAX, 0);
TEST (uadd,   unsigned char,  UCHAR_MAX, UCHAR_MAX, 0);

TEST (uadd,   unsigned short, 0U,        0U,         0);
TEST (uadd,   unsigned short, 0U,        USHRT_MAX,  0);
TEST (uadd,   unsigned short, 1U,        USHRT_MAX,  0);
TEST (uadd,   unsigned short, USHRT_MAX, USHRT_MAX,  0);

TEST (uadd,   unsigned,       0U,        0U,         0);
TEST (uadd,   unsigned,       0U,        UINT_MAX,   0);
TEST (uadd,   unsigned,       1U,        UINT_MAX,   1);
TEST (uadd,   unsigned,       UINT_MAX,  UINT_MAX,   1);

/* Unsigned long addition.  */
TEST (uaddl,  unsigned long,  0UL,       0UL,       0);
TEST (uaddl,  unsigned long,  0UL,       ULONG_MAX, 0);
TEST (uaddl,  unsigned long,  1UL,       ULONG_MAX, 1);
TEST (uaddl,  unsigned long,  ULONG_MAX, ULONG_MAX, 1);

TEST (uaddll, unsigned long long, 0ULL,       0ULL,       0);
TEST (uaddll, unsigned long long, 0ULL,       ULLONG_MAX, 0);
TEST (uaddll, unsigned long long, 1ULL,       ULLONG_MAX, 1);
TEST (uaddll, unsigned long long, ULLONG_MAX, ULLONG_MAX, 1);

/* Signed int subtraction.  */
TEST (ssub,   signed char,    0,         0,          0);
TEST (ssub,   signed char,    0,         SCHAR_MAX,  0);
TEST (ssub,   signed char,    1,         SCHAR_MAX,  0);
TEST (ssub,   signed char,    SCHAR_MAX, SCHAR_MAX,  0);
TEST (ssub,   signed char,    0,         SCHAR_MIN,  0);
TEST (ssub,   signed char,   -1,         SCHAR_MIN,  0);

TEST (ssub,   short,          0,         0,          0);
TEST (ssub,   short,          0,         SHRT_MAX,   0);
TEST (ssub,   short,          1,         SHRT_MAX,   0);
TEST (ssub,   short,          SHRT_MAX,  SHRT_MAX,   0);
TEST (ssub,   short,          0,         SHRT_MIN,   0);
TEST (ssub,   short,         -1,         SHRT_MIN,   0);
TEST (ssub,   short,          SHRT_MIN,  SHRT_MIN,   0);

TEST (ssub,   int,            0,         0,          0);
TEST (ssub,   int,            0,         INT_MAX,    0);
TEST (ssub,   int,            1,         INT_MAX,    0);
TEST (ssub,   int,            INT_MAX,   INT_MAX,    0);
TEST (ssub,   int,            0,         INT_MIN,    1);
TEST (ssub,   int,           -1,         INT_MIN,    0);
TEST (ssub,   int,            INT_MIN,   INT_MIN,    0);

/* Signed long subtraction.  */
TEST (ssubl,  long,           0L,        0L,         0);
TEST (ssubl,  long,           0L,        LONG_MAX,   0);
TEST (ssubl,  long,           1L,        LONG_MAX,   0);
TEST (ssubl,  long,           LONG_MAX,  LONG_MAX,   0);
TEST (ssubl,  long,           0L,        LONG_MIN,   1);
TEST (ssubl,  long,          -1L,        LONG_MIN,   0);
TEST (ssubl,  long,           LONG_MIN,  LONG_MIN,   0);

/* Signed long long subtraction.  */
TEST (ssubll, long long,      0LL,       0LL,        0);
TEST (ssubll, long long,      0LL,       LLONG_MAX,  0);
TEST (ssubll, long long,      1LL,       LLONG_MAX,  0);
TEST (ssubll, long long,      LLONG_MAX, LLONG_MAX,  0);
TEST (ssubll, long long,      0LL,       LLONG_MIN,  1);
TEST (ssubll, long long,     -1LL,       LLONG_MIN,  0);
TEST (ssubll, long long,      LLONG_MIN, LLONG_MIN,  0);

/* Unsigned int subtraction.  */
TEST (usub,   unsigned char,  0U,        0U,         0);
TEST (usub,   unsigned char,  0U,        UCHAR_MAX,  1);
TEST (usub,   unsigned char,  1U,        UCHAR_MAX,  1);
TEST (usub,   unsigned char,  UCHAR_MAX, UCHAR_MAX,  0);

TEST (usub,   unsigned short, 0U,        0U,         0);
TEST (usub,   unsigned short, 0U,        USHRT_MAX,  1);
TEST (usub,   unsigned short, 1U,        USHRT_MAX,  1);
TEST (usub,   unsigned short, USHRT_MAX, USHRT_MAX,  0);

TEST (usub,   unsigned,       0U,        0U,         0);
TEST (usub,   unsigned,       0U,        UINT_MAX,   1);
TEST (usub,   unsigned,       1U,        UINT_MAX,   1);
TEST (usub,   unsigned,       UINT_MAX,  UINT_MAX,   0);

/* Unsigned long subtraction.  */
TEST (usubl,  unsigned long,  0UL,       0UL,       0);
TEST (usubl,  unsigned long,  0UL,       ULONG_MAX, 1);
TEST (usubl,  unsigned long,  1UL,       ULONG_MAX, 1);
TEST (usubl,  unsigned long,  ULONG_MAX, ULONG_MAX, 0);

/* Unsigned long long subtraction.  */
TEST (usubll, unsigned long long,  0ULL,       0ULL,       0);
TEST (usubll, unsigned long long,  0ULL,       ULLONG_MAX, 1);
TEST (usubll, unsigned long long,  1ULL,       ULLONG_MAX, 1);
TEST (usubll, unsigned long long,  ULLONG_MAX, ULLONG_MAX, 0);
