/* Test __builtin_{add,sub,mul}_overflow_p.  */
/* { dg-do run { target c } } */
/* { dg-skip-if "" { ! run_expensive_tests }  { "*" } { "-O0" "-O2" } } */

#include "builtin-arith-overflow.h"

#ifdef __SIZEOF_INT128__
#define WTYPE __int128
#else
#define WTYPE long long int
#endif

struct S
{
  signed int s1 : 1;
  unsigned int u1 : 1;
  signed int s2 : 2;
  unsigned int u2 : 2;
  signed int s3 : 3;
  unsigned int u3 : 3;
  signed int s4 : 4;
  unsigned int u4 : 4;
  signed int s5 : 5;
  unsigned int u5 : 5;
  signed int s6 : 6;
  unsigned int u6 : 6;
  signed int s7 : 7;
  unsigned int u7 : 7;
} vs;

#define TESTS \
TP (100, signed char, signed char, vs.u2, -1, 0, add, 1) \
TP (101, unsigned char, unsigned char, vs.u4, 5, 5, add, 0) \
TP (102, unsigned char, unsigned char, vs.u3, 5, 3, add, 1) \
TP (103, signed char, unsigned short, vs.u1, 5, 5, sub, 0) \
TP (104, signed char, unsigned short, vs.u1, 6, 5, sub, 0) \
TP (105, signed char, unsigned short, vs.u1, 7, 5, sub, 1) \
TP (106, signed char, unsigned short, vs.u4, 5, 6, sub, 1) \
TP (107, signed char, signed char, vs.u1, -1, -1, mul, 0) \
TP (108, signed char, signed char, vs.s1, -1, -1, mul, 1) \
TP (109, unsigned char, signed char, vs.u6, 17, -2, mul, 1) \
TP (110, unsigned char, signed char, vs.s6, 17, -2, mul, 1) \
TP (111, unsigned char, signed char, vs.s7, 17, -2, mul, 0) \
TP (112, unsigned WTYPE, signed WTYPE, vs.s5, 5, -2, mul, 0) \
TP (113, unsigned WTYPE, signed WTYPE, vs.s4, 5, -2, mul, 1) \
TP (114, long long int, long long int, vs.u2, -3, 5, add, 0) \
TP (115, long long int, long long int, vs.u1, -3, 5, add, 1) \
TP (116, long long int, int, vs.u3, -5, 3, add, 1) \
TP (117, long long int, int, vs.s1, -5, 3, add, 1) \
TP (118, long long int, int, vs.s2, -5, 3, add, 0) \
TP (119, int, WTYPE, vs.u2, -3, 5, add, 0) \
TP (120, int, WTYPE, vs.u1, -3, 5, add, 1) \
TP (121, unsigned char, unsigned char, vs.u6, SCHAR_MAX - 1, (unsigned char) SCHAR_MAX + 4, sub, 1) \
TP (122, unsigned char, unsigned char, vs.s3, SCHAR_MAX - 1, (unsigned char) SCHAR_MAX + 4, sub, 1) \
TP (123, unsigned char, unsigned char, vs.s4, SCHAR_MAX - 1, (unsigned char) SCHAR_MAX + 4, sub, 0) \
TP (124, unsigned int, unsigned int, vs.u7, INT_MAX, 1, add, 1) \
TP (125, unsigned int, unsigned int, vs.u7, 127, 1, add, 1) \
TP (126, unsigned int, unsigned int, vs.u7, 1, 63, add, 0) \
TP (127, int, int, vs.s7, INT_MIN, 1, sub, 1) \
TP (128, int, int, vs.s7, -64, 1, sub, 1) \
TP (129, int, int, vs.s7, -63, 1, sub, 0)

TESTS

#undef TP
#define TP(n, t1, t2, er, v1, v2, b, o) t##n##b ();

int
main ()
{
  TESTS
  return 0;
}
