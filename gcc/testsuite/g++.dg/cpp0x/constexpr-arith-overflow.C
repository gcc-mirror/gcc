// PR c++/70507 - integer overflow builtins not constant expressions
// { dg-do compile { target c++11 } }

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

#define Assert(expr) static_assert ((expr), #expr)

template <class T>
constexpr T add (T x, T y, T z = T ())
{
  return __builtin_add_overflow (x, y, &z) ? 0 : z;
}

template <class T>
constexpr T sub (T x, T y, T z = T ())
{
  return __builtin_sub_overflow (x, y, &z) ? 0 : z;
}

template <class T>
constexpr T mul (T x, T y, T z = T ())
{
  return __builtin_mul_overflow (x, y, &z) ? 0 : z;
}

#define TEST_ADD(T, x, y, z) Assert (z == add<T>(x, y))
#define TEST_SUB(T, x, y, z) Assert (z == sub<T>(x, y))
#define TEST_MUL(T, x, y, z) Assert (z == mul<T>(x, y))


TEST_ADD (signed char,  0,         0,         0);
TEST_ADD (signed char,  0,         SCHAR_MAX, SCHAR_MAX);
TEST_ADD (signed char,  1,         SCHAR_MAX, 0);           // overflow
TEST_ADD (signed char,  SCHAR_MAX, SCHAR_MAX, 0);           // overflow
TEST_ADD (signed char,  0,         SCHAR_MIN, SCHAR_MIN);
TEST_ADD (signed char, -1,         SCHAR_MIN, 0);           // overflow

TEST_ADD (short,        0,         0,         0);
TEST_ADD (short,        0,         SHRT_MAX,  SHRT_MAX);
TEST_ADD (short,        1,         SHRT_MAX,  0);           // overflow
TEST_ADD (short,        SHRT_MAX,  SHRT_MAX,  0);           // overflow
TEST_ADD (short,        0,         SHRT_MIN,  SHRT_MIN);
TEST_ADD (short,       -1,         SHRT_MIN,  0);           // overflow
TEST_ADD (short,        SHRT_MIN,  SHRT_MIN,  0);           // overflow

TEST_ADD (int,          0,         0,         0);
TEST_ADD (int,          0,         INT_MAX,   INT_MAX);
TEST_ADD (int,          1,         INT_MAX,   0);           // overflow
TEST_ADD (int,          INT_MAX,   INT_MAX,   0);           // overflow
TEST_ADD (int,          0,         INT_MIN,   INT_MIN);
TEST_ADD (int,         -1,         INT_MIN,   0);           // overflow
TEST_ADD (int,          INT_MIN,   INT_MIN,   0);           // overflow

TEST_ADD (long,         0,         0,         0);
TEST_ADD (long,         0,         LONG_MAX,  LONG_MAX);
TEST_ADD (long,         1,         LONG_MAX,  0);           // overflow
TEST_ADD (long,         LONG_MAX,  LONG_MAX,  0);           // overflow
TEST_ADD (long,         0,         LONG_MIN,  LONG_MIN);
TEST_ADD (long,        -1,         LONG_MIN,  0);           // overflow
TEST_ADD (long,         LONG_MIN,  LONG_MIN,  0);           // overflow

TEST_ADD (long long,    0,         0,          0);
TEST_ADD (long long,    0,         LLONG_MAX,  LLONG_MAX);
TEST_ADD (long long,    1,         LLONG_MAX,  0);          // overflow
TEST_ADD (long long,    LLONG_MAX, LLONG_MAX,  0);          // overflow
TEST_ADD (long long,    0,         LLONG_MIN,  LLONG_MIN);
TEST_ADD (long long,   -1,         LLONG_MIN,  0);          // overflow
TEST_ADD (long long,    LLONG_MIN, LLONG_MIN,  0);          // overflow

TEST_ADD (unsigned char,  0,         0,         0);
TEST_ADD (unsigned char,  0,         UCHAR_MAX, UCHAR_MAX);
TEST_ADD (unsigned char,  1,         UCHAR_MAX, 0);         // overflow

TEST_ADD (unsigned char,  UCHAR_MAX, UCHAR_MAX, 0);         // overflow
TEST_ADD (unsigned short, 0,         0,          0);
TEST_ADD (unsigned short, 0,         USHRT_MAX,  USHRT_MAX);
TEST_ADD (unsigned short, 1,         USHRT_MAX,  0);        // overflow
TEST_ADD (unsigned short, USHRT_MAX, USHRT_MAX,  0);        // overflow

TEST_ADD (unsigned,       0,         0,          0);
TEST_ADD (unsigned,       0,         UINT_MAX,   UINT_MAX);
TEST_ADD (unsigned,       1,         UINT_MAX,   0);        // overflow
TEST_ADD (unsigned,       UINT_MAX,  UINT_MAX,   0);        // overflow

TEST_ADD (unsigned long,  0,         0,         0);
TEST_ADD (unsigned long,  0,         ULONG_MAX, ULONG_MAX);
TEST_ADD (unsigned long,  1,         ULONG_MAX, 0);         // overflow
TEST_ADD (unsigned long,  ULONG_MAX, ULONG_MAX, 0);         // overflow

TEST_ADD (unsigned long long,  0,          0,          0);
TEST_ADD (unsigned long long,  0,          ULLONG_MAX, ULLONG_MAX);
TEST_ADD (unsigned long long,  1,          ULLONG_MAX, 0);  // overflow
TEST_ADD (unsigned long long,  ULLONG_MAX, ULLONG_MAX, 0);  // overflow


// Make sure the built-ins are accepted in the following contexts
// where constant expressions are required and that they return
// the expected overflow value.

namespace Enum {

enum Add {
  a0 = __builtin_add_overflow_p (      1, 1, 0),
  a1 = __builtin_add_overflow_p (INT_MAX, 1, 0)
};

Assert (a0 == 0);
Assert (a1 == 1);

enum Sub {
  s0 = __builtin_sub_overflow_p (      1, 1, 0),
  s1 = __builtin_sub_overflow_p (INT_MIN, 1, 0)
};

Assert (s0 == 0);
Assert (s1 == 1);

enum Mul {
  m0 = __builtin_add_overflow_p (      1,       1, 0),
  m1 = __builtin_add_overflow_p (INT_MAX, INT_MAX, 0)
};

Assert (m0 == 0);
Assert (m1 == 1);

}   // namespace Enum

namespace TemplateArg {

template <class T, class U, class V,
	  T x, U y, bool v, bool z = __builtin_add_overflow_p (x, y, V ())>
struct Add {
  Assert (z == v);
};

template <class T, class U, class V,
	  T x, U y, bool v, bool z = __builtin_sub_overflow_p (x, y, V ())>
struct Sub {
  Assert (z == v);
};

template <class T, class U, class V,
	  T x, U y, bool v, bool z = __builtin_mul_overflow_p (x, y, V ())>
struct Mul {
  Assert (z == v);
};

template struct Add<int, int, int,  1,       1, false>;
template struct Add<int, int, int,  1, INT_MAX, true>;

template struct Sub<int, int, int,  1,       1, false>;
template struct Sub<int, int, int, -2, INT_MAX, true>;

template struct Mul<int, int, int,  1,               1, false>;
template struct Mul<int, int, int,  2, INT_MAX / 2 + 1, true>;

}   // namespace TemplateArg

#if __cplusplus >= 201402L

namespace Initializer {

struct Result {
  int res;
  bool vflow;
};

constexpr Result
add_vflow (int a, int b)
{
#if 1
  Result res = { a + b, __builtin_add_overflow_p (a, b, int ()) };
#else
  // The following fails to compile because of c++/71391 - error
  // on aggregate initialization with side-effects in a constexpr
  // function
  int c = 0;
  Result res = { 0, __builtin_add_overflow (a, b, &c) };
  res.c = c;
#endif
  return res;
}

constexpr Result sum = add_vflow (123, 456);
Assert (sum.res == 123 + 456);
Assert (!sum.vflow);

}   // namespace Initializer

#endif   // __cplusplus >= 201402L
