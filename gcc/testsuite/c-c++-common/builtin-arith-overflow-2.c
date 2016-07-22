/* PR c/68120 - can't easily deal with integer overflow at compile time */
/* { dg-do run } */
/* { dg-additional-options "-Wno-long-long" } */
/* { dg-skip-if "Program too big" { "avr-*-*" } } */

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

/* Number of failed runtime assertions.  */
int nfails;

void __attribute__ ((noclone, noinline))
runtime_assert (int expr, int line)
{
  if (!expr)
    {
      __builtin_printf ("line %i: assertion failed\n", line);
      ++nfails;
    }
}

/* Helper macros for run-time testing.  */
#define add(x, y) ((x) + (y))
#define sadd(x, y) ((x) + (y))
#define saddl(x, y) ((x) + (y))
#define saddll(x, y) ((x) + (y))
#define uadd(x, y) ((x) + (y))
#define uaddl(x, y) ((x) + (y))
#define uaddll(x, y) ((x) + (y))
#define sub(x, y) ((x) - (y))
#define ssub(x, y) ((x) - (y))
#define ssubl(x, y) ((x) - (y))
#define ssubll(x, y) ((x) - (y))
#define usub(x, y) ((x) - (y))
#define usubl(x, y) ((x) - (y))
#define usubll(x, y) ((x) - (y))
#define mul(x, y) ((x) * (y))
#define smul(x, y) ((x) * (y))
#define smull(x, y) ((x) * (y))
#define smulll(x, y) ((x) * (y))
#define umul(x, y) ((x) * (y))
#define umull(x, y) ((x) * (y))
#define umulll(x, y) ((x) * (y))

int main (void)
{

#if __cplusplus >= 201103L
#  define StaticAssert(expr) static_assert ((expr), #expr)
#elif __STDC_VERSION__ >= 201112L
#  define StaticAssert(expr) _Static_assert ((expr), #expr)
#else
  /* The following pragma has no effect due to bug 70888 - #pragma
     diagnostic ignored -Wlong-long ineffective with __LONG_LONG_MAX__
     in c++98 mode.  */
#  pragma GCC diagnostic ignored "-Wlong-long"
#  pragma GCC diagnostic ignored "-Wunused-local-typedefs"

#  define CONCAT(a, b)  a ## b
#  define CAT(a, b)     CONCAT (a, b)
#  define StaticAssert(expr)					\
     typedef int CAT (StaticAssert_, __LINE__) [1 - 2 * !(expr)]
#endif

  /* Make extra effort to prevent constant folding seeing the constant
     values of the arguments and optimizing the run-time test into
     a constant.  */
#define RuntimeAssert(op, T, U, x, y, vflow)				\
  do {									\
    volatile T a = (x), b = (y);					\
    U c = 0;								\
    volatile int vf = __builtin_ ## op ## _overflow (a, b, &c);		\
    runtime_assert ((vf == vflow), __LINE__);				\
    if (vf == 0)							\
      runtime_assert (op (a, b) == c, __LINE__);			\
  } while (0)

  /* Verify that each call to the type-generic __builtin_op_overflow(x, y)
     yields a constant expression equal to z indicating whether or not
     the constant expression (x op y) overflows when evaluated in type T.  */
#  define G_TEST(op, T, x, y, vflow)					\
  RuntimeAssert(op, __typeof__ (op (x, y)), T, x, y, vflow);		\
  StaticAssert ((vflow) == __builtin_ ## op ## _overflow_p ((x), (y), (T)0))

  /* Addition.  */
  G_TEST (add, signed char,    0,         0,         0);
  G_TEST (add, signed char,    0,         SCHAR_MAX, 0);
  G_TEST (add, signed char,    1,         SCHAR_MAX, 1);
  G_TEST (add, signed char,    SCHAR_MAX, SCHAR_MAX, 1);
  G_TEST (add, signed char,    0,         SCHAR_MIN, 0);
  G_TEST (add, signed char,   -1,         SCHAR_MIN, 1);
  /* Verify any slicing in the result type doesn't prevent the overflow
     from being detected.  */
  G_TEST (add, signed char,    UCHAR_MAX + 1, 0,     1);
  G_TEST (add, signed char,    UCHAR_MAX + 1, 1,     1);
  G_TEST (add, signed char,    1, UCHAR_MAX + 1,     1);

  G_TEST (add, unsigned char,  0,        0,          0);
  /* Verify any slicing in the result type doesn't prevent the overflow
     from being detected.  */
  G_TEST (add, unsigned char,  UCHAR_MAX + 1, 0,     1);
  G_TEST (add, unsigned char,  UCHAR_MAX + 1, 1,     1);
  G_TEST (add, unsigned char,  1, UCHAR_MAX + 1,     1);

  G_TEST (add, short,          0,         0,         0);
  G_TEST (add, short,          0,         SHRT_MAX,  0);
  G_TEST (add, short,          1,         SHRT_MAX,  1);
  G_TEST (add, short,          SHRT_MAX,  SHRT_MAX,  1);
  G_TEST (add, short,          0,         SHRT_MIN,  0);
  G_TEST (add, short,         -1,         SHRT_MIN,  1);
  G_TEST (add, short,          SHRT_MIN,  SHRT_MIN,  1);

  G_TEST (add, int,            0,         0,         0);
  G_TEST (add, int,            0,         INT_MAX,   0);
  G_TEST (add, int,            1,         INT_MAX,   1);
  G_TEST (add, int,            INT_MAX,   INT_MAX,   1);
  G_TEST (add, int,            0,         INT_MIN,   0);
  G_TEST (add, int,           -1,         INT_MIN,   1);
  G_TEST (add, int,            INT_MIN,   INT_MIN,   1);

  G_TEST (add, long,           0,         0,         0);
  G_TEST (add, long,           0,         LONG_MAX,  0);
  G_TEST (add, long,           1,         LONG_MAX,  1);
  G_TEST (add, long,           LONG_MAX,  LONG_MAX,  1);
  G_TEST (add, long,           0,         LONG_MIN,  0);
  G_TEST (add, long,          -1,         LONG_MIN,  1);
  G_TEST (add, long,           LONG_MIN,  LONG_MIN,  1);

  G_TEST (add, long long,      0,         0,          0);
  G_TEST (add, long long,      0,         LLONG_MAX,  0);
  G_TEST (add, long long,      1,         LLONG_MAX,  1);
  G_TEST (add, long long,      LLONG_MAX, LLONG_MAX,  1);
  G_TEST (add, long long,      0,         LLONG_MIN,  0);
  G_TEST (add, long long,     -1,         LLONG_MIN,  1);
  G_TEST (add, long long,      LLONG_MIN, LLONG_MIN,  1);

  /* Subtraction */
  G_TEST (sub, unsigned char,  0,         0,          0);
  G_TEST (sub, unsigned char,  0,         UCHAR_MAX,  1);
  G_TEST (sub, unsigned char,  1,         UCHAR_MAX,  1);

  G_TEST (sub, unsigned char,  UCHAR_MAX, UCHAR_MAX,  0);
  G_TEST (sub, unsigned short, 0,         0,          0);
  G_TEST (sub, unsigned short, 0,         USHRT_MAX,  1);
  G_TEST (sub, unsigned short, 1,         USHRT_MAX,  1);
  G_TEST (sub, unsigned short, USHRT_MAX, USHRT_MAX,  0);

  G_TEST (sub, unsigned,       0,         0,          0);
  G_TEST (sub, unsigned,       0,         UINT_MAX,   1);
  G_TEST (sub, unsigned,       1,         UINT_MAX,   1);
  G_TEST (sub, unsigned,       UINT_MAX,  UINT_MAX,   0);

  G_TEST (sub, unsigned long,  0,         0,          0);
  G_TEST (sub, unsigned long,  0,         ULONG_MAX,  1);
  G_TEST (sub, unsigned long,  1,         ULONG_MAX,  1);
  G_TEST (sub, unsigned long,  ULONG_MAX, ULONG_MAX,  0);

  G_TEST (sub, unsigned long long,  0,          0,          0);
  G_TEST (sub, unsigned long long,  0,          ULLONG_MAX, 1);
  G_TEST (sub, unsigned long long,  1,          ULLONG_MAX, 1);
  G_TEST (sub, unsigned long long,  ULLONG_MAX, ULLONG_MAX, 0);

  G_TEST (sub, signed char,    0,         0,           0);
  G_TEST (sub, signed char,    0,         SCHAR_MAX,   0);
  G_TEST (sub, signed char,    1,         SCHAR_MAX,   0);
  G_TEST (sub, signed char,    SCHAR_MAX, SCHAR_MAX,   0);
  G_TEST (sub, signed char,    SCHAR_MIN,         1,   1);
  G_TEST (sub, signed char,    0,         SCHAR_MIN,   1);
  G_TEST (sub, signed char,   -1,         SCHAR_MIN,   0);

  G_TEST (sub, short,          0,         0,           0);
  G_TEST (sub, short,          0,         SHRT_MAX,    0);
  G_TEST (sub, short,          1,         SHRT_MAX,    0);
  G_TEST (sub, short,          SHRT_MAX,  SHRT_MAX,    0);
  G_TEST (sub, short,          0,         SHRT_MIN,    1);
  G_TEST (sub, short,         -1,         SHRT_MIN,    0);
  G_TEST (sub, short,          SHRT_MIN,  SHRT_MIN,    0);

  G_TEST (sub, int,            0,         0,           0);
  G_TEST (sub, int,            0,         INT_MAX,     0);
  G_TEST (sub, int,            1,         INT_MAX,     0);
  G_TEST (sub, int,            INT_MAX,   INT_MAX,     0);
  G_TEST (sub, int,            0,         INT_MIN,     1);
  G_TEST (sub, int,           -1,         INT_MIN,     0);
  G_TEST (sub, int,            INT_MIN,   INT_MIN,     0);

  G_TEST (sub, long,           0,         0,           0);
  G_TEST (sub, long,           0,         LONG_MAX,    0);
  G_TEST (sub, long,           1,         LONG_MAX,    0);
  G_TEST (sub, long,           LONG_MAX,  LONG_MAX,    0);
  G_TEST (sub, long,           0,         LONG_MIN,    1);
  G_TEST (sub, long,          -1,         LONG_MIN,    0);
  G_TEST (sub, long,           LONG_MIN,  LONG_MIN,    0);

  G_TEST (sub, long long,      0,           0,           0);
  G_TEST (sub, long long,      0,           LLONG_MAX,   0);
  G_TEST (sub, long long,      1,           LLONG_MAX,   0);
  G_TEST (sub, long long,      LLONG_MAX,   LLONG_MAX,   0);
  G_TEST (sub, long long,      0,           LLONG_MIN,   1);
  G_TEST (sub, long long,     -1,           LLONG_MIN,   0);
  G_TEST (sub, long long,      LLONG_MIN,   LLONG_MIN,   0);

  G_TEST (sub, unsigned char,  0,         0,          0);
  G_TEST (sub, unsigned char,  0,         UCHAR_MAX,  1);
  G_TEST (sub, unsigned char,  1,         UCHAR_MAX,  1);
  G_TEST (sub, unsigned char,  UCHAR_MAX, UCHAR_MAX,  0);

  G_TEST (sub, unsigned short, 0,         0,          0);
  G_TEST (sub, unsigned short, 0,         USHRT_MAX,  1);
  G_TEST (sub, unsigned short, 1,         USHRT_MAX,  1);
  G_TEST (sub, unsigned short, USHRT_MAX, USHRT_MAX,  0);

  G_TEST (sub, unsigned,       0,         0,          0);
  G_TEST (sub, unsigned,       0,         UINT_MAX,   1);
  G_TEST (sub, unsigned,       1,         UINT_MAX,   1);
  G_TEST (sub, unsigned,       UINT_MAX,  UINT_MAX,   0);

  G_TEST (sub, unsigned long,  0,         0,          0);
  G_TEST (sub, unsigned long,  0,         ULONG_MAX,  1);
  G_TEST (sub, unsigned long,  1,         ULONG_MAX,  1);
  G_TEST (sub, unsigned long,  ULONG_MAX, ULONG_MAX,  0);

  G_TEST (sub, unsigned long long,  0,          0,          0);
  G_TEST (sub, unsigned long long,  0,          ULLONG_MAX, 1);
  G_TEST (sub, unsigned long long,  1,          ULLONG_MAX, 1);
  G_TEST (sub, unsigned long long,  ULLONG_MAX, ULLONG_MAX, 0);

  /* Multiplication.  */
  G_TEST (mul, unsigned char,  0,         0,          0);
  G_TEST (mul, unsigned char,  0,         UCHAR_MAX,  0);
  G_TEST (mul, unsigned char,  1,         UCHAR_MAX,  0);
  G_TEST (mul, unsigned char,  2,         UCHAR_MAX,  1);
  G_TEST (mul, unsigned char,  UCHAR_MAX, UCHAR_MAX,  1);

  G_TEST (mul, unsigned short, 0,         0,          0);
  G_TEST (mul, unsigned short, 0,         USHRT_MAX,  0);
  G_TEST (mul, unsigned short, 1,         USHRT_MAX,  0);
  G_TEST (mul, unsigned short, USHRT_MAX, 2,          1);
  G_TEST (mul, unsigned short, USHRT_MAX, USHRT_MAX,  1);

  G_TEST (mul, unsigned,       0,         0,          0);
  G_TEST (mul, unsigned,       0,         UINT_MAX,   0);
  G_TEST (mul, unsigned,       1,         UINT_MAX,   0);
  G_TEST (mul, unsigned,       2,         UINT_MAX,   1);
  G_TEST (mul, unsigned,       UINT_MAX,  UINT_MAX,   1);

  G_TEST (mul, unsigned long,  0,         0,          0);
  G_TEST (mul, unsigned long,  0,         ULONG_MAX,  0);
  G_TEST (mul, unsigned long,  1,         ULONG_MAX,  0);
  G_TEST (mul, unsigned long,  2,         ULONG_MAX,  1);
  G_TEST (mul, unsigned long,  ULONG_MAX, ULONG_MAX,  1);

  G_TEST (mul, unsigned long long,  0,          0,          0);
  G_TEST (mul, unsigned long long,  0,          ULLONG_MAX, 0);
  G_TEST (mul, unsigned long long,  1,          ULLONG_MAX, 0);
  G_TEST (mul, unsigned long long,  2,          ULLONG_MAX, 1);
  G_TEST (mul, unsigned long long,  ULLONG_MAX, ULLONG_MAX, 1);

  G_TEST (mul, signed char,  0,         0,           0);
  G_TEST (mul, signed char,  0,         SCHAR_MAX,   0);
  G_TEST (mul, signed char,  1,         SCHAR_MAX,   0);
  G_TEST (mul, signed char,  SCHAR_MAX, SCHAR_MAX,   1);
  G_TEST (mul, signed char,  SCHAR_MIN,         1,   0);
  G_TEST (mul, signed char,  0,         SCHAR_MIN,   0);
  G_TEST (mul, signed char, -1,         SCHAR_MIN,   1);

  G_TEST (mul, short,        0,         0,           0);
  G_TEST (mul, short,        0,         SHRT_MAX,    0);
  G_TEST (mul, short,        1,         SHRT_MAX,    0);
  G_TEST (mul, short,        SHRT_MAX,  SHRT_MAX,    1);
  G_TEST (mul, short,        0,         SHRT_MIN,    0);
  G_TEST (mul, short,       -1,         SHRT_MIN,    1);
  G_TEST (mul, short,        SHRT_MIN,  SHRT_MIN,    1);

  G_TEST (mul, int,          0,         0,           0);
  G_TEST (mul, int,          0,         INT_MAX,     0);
  G_TEST (mul, int,          1,         INT_MAX,     0);
  G_TEST (mul, int,          INT_MAX,   INT_MAX,     1);
  G_TEST (mul, int,          0,         INT_MIN,     0);
  G_TEST (mul, int,         -1,         INT_MIN,     1);
  G_TEST (mul, int,          INT_MIN,   INT_MIN,     1);

  G_TEST (mul, long,         0,         0,           0);
  G_TEST (mul, long,         0,         LONG_MAX,    0);
  G_TEST (mul, long,         1,         LONG_MAX,    0);
  G_TEST (mul, long,         LONG_MAX,  LONG_MAX,    1);
  G_TEST (mul, long,         0,         LONG_MIN,    0);
  G_TEST (mul, long,        -1,         LONG_MIN,    1);
  G_TEST (mul, long,         LONG_MIN,  LONG_MIN,    1);

  G_TEST (mul, long long,    0,           0,           0);
  G_TEST (mul, long long,    0,           LLONG_MAX,   0);
  G_TEST (mul, long long,    1,           LLONG_MAX,   0);
  G_TEST (mul, long long,    LLONG_MAX,   LLONG_MAX,   1);
  G_TEST (mul, long long,    0,           LLONG_MIN,   0);
  G_TEST (mul, long long,   -1,           LLONG_MIN,   1);
  G_TEST (mul, long long,    LLONG_MIN,   LLONG_MIN,   1);

  G_TEST (mul, unsigned char,  0,         0,          0);
  G_TEST (mul, unsigned char,  0,         UCHAR_MAX,  0);
  G_TEST (mul, unsigned char,  1,         UCHAR_MAX,  0);
  G_TEST (mul, unsigned char,  UCHAR_MAX, UCHAR_MAX,  1);

  G_TEST (mul, unsigned short, 0,         0,          0);
  G_TEST (mul, unsigned short, 0,         USHRT_MAX,  0);
  G_TEST (mul, unsigned short, 1,         USHRT_MAX,  0);
  G_TEST (mul, unsigned short, USHRT_MAX, USHRT_MAX,  1);

  G_TEST (mul, unsigned,       0,         0,          0);
  G_TEST (mul, unsigned,       0,         UINT_MAX,   0);
  G_TEST (mul, unsigned,       1,         UINT_MAX,   0);
  G_TEST (mul, unsigned,       UINT_MAX,  UINT_MAX,   1);

  G_TEST (mul, unsigned long,  0,         0,          0);
  G_TEST (mul, unsigned long,  0,         ULONG_MAX,  0);
  G_TEST (mul, unsigned long,  1,         ULONG_MAX,  0);
  G_TEST (mul, unsigned long,  ULONG_MAX, ULONG_MAX,  1);

  G_TEST (mul, unsigned long long,  0,          0,          0);
  G_TEST (mul, unsigned long long,  0,          ULLONG_MAX, 0);
  G_TEST (mul, unsigned long long,  1,          ULLONG_MAX, 0);
  G_TEST (mul, unsigned long long,  ULLONG_MAX, ULLONG_MAX, 1);

  /* Verify that each call to the type-specific __builtin_op_overflow
     evaluates to a (not-necessarily constant) expression indicating
     whether or not the constant expression (x op y) overflows.
     The type-specific forms of the built-ins detect overflow after
     arithmetic promotions and so unlike the type-generic overloads
     cannot detect overflow in char or short types.  */

#define T_TEST(op, T, x, y, vflow)				\
  RuntimeAssert (op, T, __typeof__ ((x) + (y)), x, y, vflow)

  /* Signed int addition.  */
  T_TEST (sadd,   signed char,    0,         0,         0);
  T_TEST (sadd,   signed char,    0,         SCHAR_MAX, 0);
  T_TEST (sadd,   signed char,    1,         SCHAR_MAX, 0);
  T_TEST (sadd,   signed char,    SCHAR_MAX, SCHAR_MAX, 0);
  T_TEST (sadd,   signed char,    0,         SCHAR_MIN, 0);
  T_TEST (sadd,   signed char,   -1,         SCHAR_MIN, 0);

  T_TEST (sadd,   short,          0,         0,         0);
  T_TEST (sadd,   short,          0,         SHRT_MAX,  0);
  T_TEST (sadd,   short,          1,         SHRT_MAX,  0);
  T_TEST (sadd,   short,          SHRT_MAX,  SHRT_MAX,  0);
  T_TEST (sadd,   short,          0,         SHRT_MIN,  0);
  T_TEST (sadd,   short,         -1,         SHRT_MIN,  0);
  T_TEST (sadd,   short,          SHRT_MIN,  SHRT_MIN,  0);

  T_TEST (sadd,   int,            0,         0,         0);
  T_TEST (sadd,   int,            0,         INT_MAX,   0);
  T_TEST (sadd,   int,            1,         INT_MAX,   1);
  T_TEST (sadd,   int,            INT_MAX,   INT_MAX,   1);
  T_TEST (sadd,   int,            0,         INT_MIN,   0);
  T_TEST (sadd,   int,           -1,         INT_MIN,   1);
  T_TEST (sadd,   int,            INT_MIN,   INT_MIN,   1);

  /* Signed long addition.  */
  T_TEST (saddl,  long,           0L,        0L,        0);
  T_TEST (saddl,  long,           0L,        LONG_MAX,  0);
  T_TEST (saddl,  long,           1L,        LONG_MAX,  1);
  T_TEST (saddl,  long,           LONG_MAX,  LONG_MAX,  1);
  T_TEST (saddl,  long,           0L,        LONG_MIN,  0);
  T_TEST (saddl,  long,          -1L,        LONG_MIN,  1);
  T_TEST (saddl,  long,           LONG_MIN,  LONG_MIN,  1);

  T_TEST (saddll, long long,      0LL,       0LL,        0);
  T_TEST (saddll, long long,      0LL,       LLONG_MAX,  0);
  T_TEST (saddll, long long,      1LL,       LLONG_MAX,  1);
  T_TEST (saddll, long long,      LLONG_MAX, LLONG_MAX,  1);
  T_TEST (saddll, long long,      0LL,       LLONG_MIN,  0);
  T_TEST (saddll, long long,     -1LL,       LLONG_MIN,  1);
  T_TEST (saddll, long long,      LLONG_MIN, LLONG_MIN,  1);

  /* Unsigned int addition.  */
  T_TEST (uadd,   unsigned char,  0U,        0U,         0);
  T_TEST (uadd,   unsigned char,  0U,        UCHAR_MAX, 0);
  T_TEST (uadd,   unsigned char,  1U,        UCHAR_MAX, 0);
  T_TEST (uadd,   unsigned char,  UCHAR_MAX, UCHAR_MAX, 0);

  T_TEST (uadd,   unsigned short, 0U,        0U,         0);
  T_TEST (uadd,   unsigned short, 0U,        USHRT_MAX,  0);
  T_TEST (uadd,   unsigned short, 1U,        USHRT_MAX,  0);
  T_TEST (uadd,   unsigned short, USHRT_MAX, USHRT_MAX,  0);

  T_TEST (uadd,   unsigned,       0U,        0U,         0);
  T_TEST (uadd,   unsigned,       0U,        UINT_MAX,   0);
  T_TEST (uadd,   unsigned,       1U,        UINT_MAX,   1);
  T_TEST (uadd,   unsigned,       UINT_MAX,  UINT_MAX,   1);

  /* Unsigned long addition.  */
  T_TEST (uaddl,  unsigned long,  0UL,       0UL,       0);
  T_TEST (uaddl,  unsigned long,  0UL,       ULONG_MAX, 0);
  T_TEST (uaddl,  unsigned long,  1UL,       ULONG_MAX, 1);
  T_TEST (uaddl,  unsigned long,  ULONG_MAX, ULONG_MAX, 1);

  T_TEST (uaddll, unsigned long long, 0ULL,       0ULL,       0);
  T_TEST (uaddll, unsigned long long, 0ULL,       ULLONG_MAX, 0);
  T_TEST (uaddll, unsigned long long, 1ULL,       ULLONG_MAX, 1);
  T_TEST (uaddll, unsigned long long, ULLONG_MAX, ULLONG_MAX, 1);

  /* Signed int subtraction.  */
  T_TEST (ssub,   signed char,    0,         0,          0);
  T_TEST (ssub,   signed char,    0,         SCHAR_MAX,  0);
  T_TEST (ssub,   signed char,    1,         SCHAR_MAX,  0);
  T_TEST (ssub,   signed char,    SCHAR_MAX, SCHAR_MAX,  0);
  T_TEST (ssub,   signed char,    0,         SCHAR_MIN,  0);
  T_TEST (ssub,   signed char,   -1,         SCHAR_MIN,  0);

  T_TEST (ssub,   short,          0,         0,          0);
  T_TEST (ssub,   short,          0,         SHRT_MAX,   0);
  T_TEST (ssub,   short,          1,         SHRT_MAX,   0);
  T_TEST (ssub,   short,          SHRT_MAX,  SHRT_MAX,   0);
  T_TEST (ssub,   short,          0,         SHRT_MIN,   0);
  T_TEST (ssub,   short,         -1,         SHRT_MIN,   0);
  T_TEST (ssub,   short,          SHRT_MIN,  SHRT_MIN,   0);

  T_TEST (ssub,   int,            0,         0,          0);
  T_TEST (ssub,   int,            0,         INT_MAX,    0);
  T_TEST (ssub,   int,            1,         INT_MAX,    0);
  T_TEST (ssub,   int,            INT_MAX,   INT_MAX,    0);
  T_TEST (ssub,   int,            0,         INT_MIN,    1);
  T_TEST (ssub,   int,           -1,         INT_MIN,    0);
  T_TEST (ssub,   int,            INT_MIN,   INT_MIN,    0);

  /* Signed long subtraction.  */
  T_TEST (ssubl,  long,           0L,        0L,         0);
  T_TEST (ssubl,  long,           0L,        LONG_MAX,   0);
  T_TEST (ssubl,  long,           1L,        LONG_MAX,   0);
  T_TEST (ssubl,  long,           LONG_MAX,  LONG_MAX,   0);
  T_TEST (ssubl,  long,           0L,        LONG_MIN,   1);
  T_TEST (ssubl,  long,          -1L,        LONG_MIN,   0);
  T_TEST (ssubl,  long,           LONG_MIN,  LONG_MIN,   0);

  /* Signed long long subtraction.  */
  T_TEST (ssubll, long long,      0LL,       0LL,        0);
  T_TEST (ssubll, long long,      0LL,       LLONG_MAX,  0);
  T_TEST (ssubll, long long,      1LL,       LLONG_MAX,  0);
  T_TEST (ssubll, long long,      LLONG_MAX, LLONG_MAX,  0);
  T_TEST (ssubll, long long,      0LL,       LLONG_MIN,  1);
  T_TEST (ssubll, long long,     -1LL,       LLONG_MIN,  0);
  T_TEST (ssubll, long long,      LLONG_MIN, LLONG_MIN,  0);

  /* Unsigned int subtraction.  */
  T_TEST (usub,   unsigned char,  0U,        0U,         0);
  T_TEST (usub,   unsigned char,  0U,        UCHAR_MAX,  1);
  T_TEST (usub,   unsigned char,  1U,        UCHAR_MAX,  1);
  T_TEST (usub,   unsigned char,  UCHAR_MAX, UCHAR_MAX,  0);

  T_TEST (usub,   unsigned short, 0U,        0U,         0);
  T_TEST (usub,   unsigned short, 0U,        USHRT_MAX,  1);
  T_TEST (usub,   unsigned short, 1U,        USHRT_MAX,  1);
  T_TEST (usub,   unsigned short, USHRT_MAX, USHRT_MAX,  0);

  T_TEST (usub,   unsigned,       0U,        0U,         0);
  T_TEST (usub,   unsigned,       0U,        UINT_MAX,   1);
  T_TEST (usub,   unsigned,       1U,        UINT_MAX,   1);
  T_TEST (usub,   unsigned,       UINT_MAX,  UINT_MAX,   0);

  /* Unsigned long subtraction.  */
  T_TEST (usubl,  unsigned long,  0UL,       0UL,       0);
  T_TEST (usubl,  unsigned long,  0UL,       ULONG_MAX, 1);
  T_TEST (usubl,  unsigned long,  1UL,       ULONG_MAX, 1);
  T_TEST (usubl,  unsigned long,  ULONG_MAX, ULONG_MAX, 0);

  /* Unsigned long long subtraction.  */
  T_TEST (usubll, unsigned long long,  0ULL,       0ULL,       0);
  T_TEST (usubll, unsigned long long,  0ULL,       ULLONG_MAX, 1);
  T_TEST (usubll, unsigned long long,  1ULL,       ULLONG_MAX, 1);
  T_TEST (usubll, unsigned long long,  ULLONG_MAX, ULLONG_MAX, 0);

  return 0;
}
