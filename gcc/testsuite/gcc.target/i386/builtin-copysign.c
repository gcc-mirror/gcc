/* { dg-do compile } */
/* { dg-options "-O2" } */

#define TEST_SET(MODE, CEXT)						\
MODE test1##CEXT(MODE a) { return -a; }					\
MODE test2##CEXT(MODE a) { return __builtin_fabs##CEXT(a); }		\
MODE test3##CEXT(MODE a) { return __builtin_copysign##CEXT(a, 0.0); }	\
MODE test4##CEXT(MODE a) { return __builtin_copysign##CEXT(a, -1.0); }	\
MODE test5##CEXT(MODE a, MODE b) { return __builtin_copysign##CEXT(a, b); }

TEST_SET (float, f)
TEST_SET (double, )
TEST_SET (long double, l)
TEST_SET (__float128, q)
