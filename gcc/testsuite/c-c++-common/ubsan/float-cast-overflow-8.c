/* { dg-do run } */
/* { dg-skip-if "" { ! run_expensive_tests } { "*" } { "-O2" } } */
/* { dg-options "-fsanitize=float-cast-overflow -fsanitize-recover -DUSE_FLT_DBL_LDBL" } */
/* { dg-additional-options "-DUSE_INT128" { target int128 } } */

#include "float-cast-overflow-7.h"

#define TEST(type1, type2) \
  if (type1##_MIN)						\
    {								\
      type2 min = type1##_MIN;					\
      type2 add = -1.0;						\
      while (1)							\
	{							\
	  volatile type2 tem = min + add;			\
	  if (tem != min)					\
	    {							\
	      volatile type1 tem3 = cvt_##type1##_##type2 (tem);\
	      break;						\
	    }							\
	  add = add * type2##_RADIX;				\
	  if (min == add)					\
	    break;						\
	}							\
    }								\
  else								\
    {								\
      volatile type1 tem3 = cvt_##type1##_##type2 (-1.0f);	\
    }								\
  {								\
    type2 max = type1##_MAX;					\
    type2 add = 1.0;						\
    while (1)							\
      {								\
	volatile type2 tem = max + add;				\
	if (tem != max)						\
	  {							\
	    volatile type1 tem3 = cvt_##type1##_##type2 (tem);	\
	    break;						\
	  }							\
	add = add * type2##_RADIX;				\
	if (max == add)						\
	  break;						\
      }								\
  }

#ifdef si128_MAX
# define TESTS128(type2) TEST (si128, type2) TEST (ui128, type2)
#else
# define TESTS128(type2)
#endif

#define TESTS(type2) \
  TEST (sc, type2) TEST (c, type2) TEST (uc, type2)	\
  TEST (ss, type2) TEST (us, type2)			\
  TEST (si, type2) TEST (ui, type2)			\
  TEST (sl, type2) TEST (ul, type2)			\
  TEST (sll, type2) TEST (ull, type2)			\
  TESTS128 (type2)

int
main ()
{
#ifdef f_MAX
  TESTS (f)
#endif
#ifdef d_MAX
  TESTS (d)
#endif
#ifdef ld_MAX
  TESTS (ld)
#endif
#ifdef f80_MAX
  TESTS (f80)
#endif
#ifdef f128_MAX
  TESTS (f128)
#endif
#ifdef BROKEN_DECIMAL_INT128
# undef TESTS128
# define TESTS128(type2)
# undef TWO
# undef M1U
# undef MAXS
# define TWO 2ULL
# define M1U -1ULL
# define MAXS (__CHAR_BIT__ * __SIZEOF_LONG_LONG__)
#endif
#ifdef d32_MAX
  TESTS (d32)
#endif
#ifdef d64_MAX
  TESTS (d64)
#endif
#ifdef d128_MAX
  TESTS (d128)
#endif
  return 0;
}

/* float */
/* { dg-output "value -129 is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "value \[0-9.e+-]* is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value 128 is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value (-129|-1) is outside the range of representable values of type 'char'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'char'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value (128|256) is outside the range of representable values of type 'char'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'char'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 256 is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value -32769 is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value 32768 is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 65536 is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" { target { int128 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" { target { int128 } } } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" { target { int128 } } } */
/* No error for float and __int128 unsigned max value, as ui128_MAX is +Inf in float.  */
/* double */
/* { dg-output "\[^\n\r]*value -129 is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value 128 is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value (-129|-1) is outside the range of representable values of type 'char'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'char'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value (128|256) is outside the range of representable values of type 'char'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'char'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 256 is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value -32769 is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value 32768 is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 65536 is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" { target { int128 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" { target { int128 } } } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" { target { int128 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" { target { int128 } } } */
/* long double */
/* { dg-output "\[^\n\r]*value -129 is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value 128 is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'signed char'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value (-129|-1) is outside the range of representable values of type 'char'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'char'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value (128|256) is outside the range of representable values of type 'char'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'char'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 256 is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'unsigned char'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value -32769 is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value 32768 is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'short int'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value 65536 is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" { target { ilp32 || lp64 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'short unsigned int'\[^\n\r]*(\n|\r\n|\r)" { target { ! { ilp32 || lp64 } } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long long int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type 'long long unsigned int'\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" { target { int128 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type '__int128'\[^\n\r]*(\n|\r\n|\r)" { target { int128 } } } */
/* { dg-output "\[^\n\r]*value -1 is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" { target { int128 } } } */
/* { dg-output "\[^\n\r]*value \[0-9.e+-]* is outside the range of representable values of type '__int128 unsigned'\[^\n\r]*(\n|\r\n|\r)" { target { int128 } } } */
