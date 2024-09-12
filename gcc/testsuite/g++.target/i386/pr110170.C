/* { dg-do run { target sse4 } } */
/* { dg-options " -O2 -msse4.1 -mfpmath=sse -std=gnu++20" } */
/* { dg-skip-if "requires hosted libstdc++ for cmath" { ! hostedlib } } */

#include <math.h>

#ifndef CHECK_H
#define CHECK_H "sse4_1-check.h"
#endif

#ifndef TEST
#define TEST sse4_1_test
#endif

#include CHECK_H

void
__attribute__((noinline))
__cond_swap(double* __x, double* __y) {
  bool __r = (*__x < *__y);
  auto __tmp = __r ? *__x : *__y;
  *__y = __r ? *__y : *__x;
  *__x = __tmp;
}

auto test1() {
    double nan = -0.0;
    double x = 0.0;
    __cond_swap(&nan, &x);
    return x == -0.0 && nan == 0.0;
}

auto test1r() {
    double nan = NAN;
    double x = 1.0;
    __cond_swap(&x, &nan);
    return isnan(x) && signbit(x) == 0 && nan == 1.0;
}

auto test2() {
    double nan = NAN;
    double x = -1.0;
    __cond_swap(&nan, &x);
    return isnan(x) && signbit(x) == 0 && nan == -1.0;
}

auto test2r() {
    double nan = NAN;
    double x = -1.0;
    __cond_swap(&x, &nan);
    return isnan(x) && signbit(x) == 0 && nan == -1.0;
}

auto test3() {
    double nan = -NAN;
    double x = 1.0;
    __cond_swap(&nan, &x);
    return isnan(x) && signbit(x) == 1 && nan == 1.0;
}

auto test3r() {
    double nan = -NAN;
    double x = 1.0;
    __cond_swap(&x, &nan);
    return isnan(x) && signbit(x) == 1 && nan == 1.0;
}

auto test4() {
    double nan = -NAN;
    double x = -1.0;
    __cond_swap(&nan, &x);
    return isnan(x) && signbit(x) == 1 && nan == -1.0;
}

auto test4r() {
    double nan = -NAN;
    double x = -1.0;
    __cond_swap(&x, &nan);
    return isnan(x) && signbit(x) == 1 && nan == -1.0;
}


static void
TEST()
{
  if (
      !test1() || !test1r()
      || !test2() || !test2r()
      || !test3() || !test3r()
      || !test4() || !test4r()
      ) __builtin_abort();
}
