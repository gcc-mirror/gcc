/* Test __builtin_{add,sub,mul}_overflow.  */
/* { dg-do run } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */

#include "builtin-arith-overflow.h"

#ifdef __SIZEOF_INT128__
#define WTYPE __int128
#else
#define WTYPE long long int
#endif

#define TESTS \
T (100, signed char, signed char, unsigned WTYPE, -1, 0, -1, add, 1) \
T (101, unsigned char, unsigned char, unsigned WTYPE, 5, 5, 10, add, 0) \
T (102, signed char, unsigned short, unsigned WTYPE, 5, 5, 0, sub, 0) \
T (103, signed char, unsigned short, unsigned WTYPE, 5, 6, -1, sub, 1) \
T (104, signed char, signed char, unsigned WTYPE, -1, -1, 1, mul, 0) \
T (105, unsigned char, signed char, unsigned WTYPE, 17, -2, -34, mul, 1) \
T (106, unsigned WTYPE, signed WTYPE, signed char, 5, -2, -10, mul, 0) \
T (107, long long int, long long int, unsigned char, -3, 5, 2, add, 0) \
T (108, long long int, int, unsigned char, -5, 3, -2, add, 1) \
T (109, int, WTYPE, unsigned char, -3, 5, 2, add, 0) \
T (110, unsigned char, unsigned char, unsigned WTYPE, SCHAR_MAX - 1, (unsigned char) SCHAR_MAX + 4, -5, sub, 1)

TESTS

#undef T
#define T(n, t1, t2, tr, v1, v2, vr, b, o) t##n##b ();

int
main ()
{
  TESTS
  return 0;
}
