/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-Ofast --param aarch64-autovec-preference=asimd-only" } */
/* { dg-require-effective-target lp64 } */

#include <stdio.h>
#include <math.h>

#define N 640
#ifndef TYPE
#define TYPE float
#endif
#ifndef FMT
#define FMT ".6f"
#endif

TYPE a[N] = {0};
TYPE b[N] = {0};

char *curr_test;

/* Macro to define a function with a specific comparison */
#define DEFINE_TEST_FUNC(NAME, OP)            \
  __attribute__((noipa))		      \
  void NAME(void) {                           \
    for (int i = 0; i < N; i++) {             \
      b[i] += a[i];                           \
      if (a[i] OP 0)                          \
        break;                                \
    }                                         \
  }

/* Generate comparison functions */
DEFINE_TEST_FUNC(f1, >)
DEFINE_TEST_FUNC(f2, >=)
DEFINE_TEST_FUNC(f3, ==)
DEFINE_TEST_FUNC(f4, !=)
DEFINE_TEST_FUNC(f5, <)
DEFINE_TEST_FUNC(f6, <=)

/* Example unordered-sensitive loop: breaks if a[i] is unordered with 0 */
__attribute__((noipa))
void f7(void) {
  for (int i = 0; i < N; i++) {
    b[i] += a[i];
    if (__builtin_isunordered(a[i], 0.0f))
      break;
  }
}

__attribute__((noreturn))
static inline void __abort_trace(const char *m, int i, TYPE result, TYPE expected) {
  printf("*** [%s] FAIL AT %s:%d in %s - expected %" FMT " but got %" FMT " at pos %d\n",
         m, __FILE__, __LINE__, curr_test, expected, result, i);
  __builtin_abort();
}

/* Array setup */
#define RESET_ARRAYS(_aval, _idx, _force, _bval)         \
  do {                                                   \
    _Pragma("GCC novector")				 \
    for (int i = 0; i < N; ++i) {                        \
      a[i] = _aval;                                      \
      b[i] = _bval;                                      \
    }                                                    \
    if (_idx >= 0 && _idx < N)                           \
      a[_idx] = _force;                                  \
  } while (0)

/* Floating-point comparison macros (with unordered handling) */
#define CHECK_EQ(_i, _val) do {                                  \
  if (__builtin_isnan (_val) != __builtin_isnan (b[_i])          \
      && b[_i] != _val)                                          \
    __abort_trace ("single", _i, b[_i], _val);                   \
} while (0)

#define CHECK_RANGE_EQ(_start, _end, _val) do {                  \
  _Pragma("GCC novector")                                        \
  for (int i = _start; i < _end; ++i)                            \
    if (__builtin_isnan (_val) != __builtin_isnan (b[i])         \
	&& b[i] != _val)                                         \
      __abort_trace ("range", i, b[i], _val);                    \
} while (0)

#define str(s) #s
#define TEST_FUNC(_func, _aval, _idx, _force, _bval, _check_stmt)  \
  do {                                                             \
    curr_test = str (_func);                                       \
    RESET_ARRAYS((_aval), (_idx), (_force), (_bval));              \
    _func();                                                       \
    _check_stmt;                                                   \
  } while (0)

int main(void) {
  /* Break on random intervals.  */
  TEST_FUNC(f1, 1.0f, 0, 1.0f, 10.0f, CHECK_EQ(0, 11.0f); CHECK_EQ(1, 10.0f));
  TEST_FUNC(f2, -1.0f, 5, 0.0f, 10.0f, CHECK_EQ(0, 9.0f); CHECK_EQ(5, 10.0f));
  TEST_FUNC(f3, 3.0f, 3, 0.0f, 0.0f, CHECK_EQ(0, 3.0f); CHECK_EQ(3, 0.0f));
  TEST_FUNC(f4, 0.0f, 4, 1.0f, 1.0f, CHECK_EQ(4, 2.0f); CHECK_EQ(5, 1.0f));
  TEST_FUNC(f5, 1.0f, 6, -1.0f, 5.0f, CHECK_EQ(6, 4.0f); CHECK_EQ(7, 5.0f));
  TEST_FUNC(f6, 2.0f, 10, 0.0f, 7.0f, CHECK_EQ(10, 7.0f); CHECK_EQ(11, 7.0f));

  /* Break on last iteration.  */
  TEST_FUNC(f1, 0.0f, N - 1, 1.0f, 1.0f,
    CHECK_RANGE_EQ(0, N - 1, 1.0f); CHECK_EQ(N - 1, 2.0f));

  TEST_FUNC(f2, -5.0f, N - 1, 0.0f, 9.0f,
    CHECK_RANGE_EQ(0, N - 1, 4.0f); CHECK_EQ(N - 1, 9.0f));

  TEST_FUNC(f3, 2.0f, N - 1, 0.0f, 0.0f,
    CHECK_RANGE_EQ(0, N - 1, 2.0f); CHECK_EQ(N - 1, 0.0f));

  TEST_FUNC(f4, 0.0f, N - 1, 2.0f, 1.0f,
    CHECK_RANGE_EQ(0, N - 1, 1.0f); CHECK_EQ(N - 1, 3.0f));

  TEST_FUNC(f5, 2.0f, N - 1, -3.0f, 6.0f,
    CHECK_RANGE_EQ(0, N - 1, 8.0f); CHECK_EQ(N - 1, 3.0f));

  TEST_FUNC(f6, 5.0f, N - 1, 0.0f, 7.0f,
    CHECK_RANGE_EQ(0, N - 1, 12.0f); CHECK_EQ(N - 1, 7.0f));

  /* Condition never met — full loop executes.  */
  TEST_FUNC(f1, 0.0f, -1, 0.0f, 2.0f,
    CHECK_RANGE_EQ(0, N, 2.0f));

  TEST_FUNC(f2, -2.0f, -1, 0.0f, 5.0f,
    CHECK_RANGE_EQ(0, N, 3.0f));

  TEST_FUNC(f3, 1.0f, -1, 0.0f, 0.0f,
    CHECK_RANGE_EQ(0, N, 1.0f));

  TEST_FUNC(f4, 0.0f, -1, 0.0f, 7.0f,
    CHECK_RANGE_EQ(0, N, 7.0f));

  TEST_FUNC(f5, 1.0f, -1, 0.0f, 4.0f,
    CHECK_RANGE_EQ(0, N, 5.0f));

  TEST_FUNC(f6, 5.0f, -1, 0.0f, 3.0f,
    CHECK_RANGE_EQ(0, N, 8.0f));

#if !defined(__FAST_MATH__)
  /* Unordered break (NAN in a[i]) */
  TEST_FUNC(f7, 1.0f, 123, NAN, 2.0f,
    CHECK_RANGE_EQ(0, 123, 3.0f); CHECK_EQ(123, NAN));
#endif

  return 0;
}
