/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-additional-options "-O3 -march=rv64gcv -mabi=lp64d -std=gnu99" } */

#include <stdio.h>

#define N 640
#ifndef TYPE
#define TYPE int
#endif

TYPE a[N] = {0};
TYPE b[N] = {0};

char *curr_test;

#define DEFINE_TEST_FUNC(NAME, OP)            \
  __attribute__((noipa))		      \
  void NAME(void) {                           \
    for (int i = 0; i < N; i++) {             \
      b[i] += a[i];                           \
      if (a[i] OP 0)                          \
        break;                                \
    }                                         \
  }

DEFINE_TEST_FUNC(f1, >)
DEFINE_TEST_FUNC(f2, >=)
DEFINE_TEST_FUNC(f3, ==)
DEFINE_TEST_FUNC(f4, !=)
DEFINE_TEST_FUNC(f5, <)
DEFINE_TEST_FUNC(f6, <=)

/* Array setup macro.  */
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

/* Value check macros.  */
#define CHECK_EQ(_i, _val)                        \
  do {                                            \
    if (b[_i] != _val)                            \
      __builtin_abort ();			  \
  } while (0)

#define CHECK_RANGE_EQ(_start, _end, _val)               \
  do {                                                   \
    _Pragma("GCC novector")				 \
    for (int i = _start; i < _end; ++i)                  \
      if (b[i] != _val)                                  \
	__builtin_abort ();				 \
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
  TEST_FUNC (f1, 1, 0, 1, 10, CHECK_EQ (0, 11); CHECK_EQ (1, 10));
  TEST_FUNC (f2, -1, 5, 0, 10, CHECK_EQ (0, 9); CHECK_EQ (5, 10));
  TEST_FUNC (f3, 3, 3, 0, 0, CHECK_EQ (0, 3); CHECK_EQ (3, 0));
  TEST_FUNC (f4, 0, 4, 1, 1, CHECK_EQ (4, 2); CHECK_EQ (5, 1));
  TEST_FUNC (f5, 1, 6, -1, 5, CHECK_EQ (6, 4); CHECK_EQ (7, 5));
  TEST_FUNC (f6, 2, 10, 0, 7, CHECK_EQ (10, 7); CHECK_EQ (11, 7));

  /* Break on last iteration.  */
  TEST_FUNC (f1, 0, N-1, 1, 1,
    CHECK_RANGE_EQ (0, N-1, 1); CHECK_EQ (N-1, 2));

  TEST_FUNC (f2, -5, N-1, 0, 9,
    CHECK_RANGE_EQ (0, N-1, 4); CHECK_EQ (N-1, 9));

  TEST_FUNC (f3, 2, N-1, 0, 0,
    CHECK_RANGE_EQ(0, N-1, 2); CHECK_EQ (N-1, 0));

  TEST_FUNC (f4, 0, N-1, 2, 1,
    CHECK_RANGE_EQ (0, N-1, 1); CHECK_EQ (N-1, 3));

  TEST_FUNC (f5, 2, N-1, -3, 6,
    CHECK_RANGE_EQ (0, N-1, 8); CHECK_EQ (N-1, 3));

  TEST_FUNC (f6, 5, N-1, 0, 7,
    CHECK_RANGE_EQ (0, N-1, 12); CHECK_EQ (N-1, 7));

  TEST_FUNC (f1, 0, -1, 0, 2,
    CHECK_RANGE_EQ (0, N, 2));

  TEST_FUNC (f2, -2, -1, 0, 5,
    CHECK_RANGE_EQ (0, N, 3));

  TEST_FUNC (f3, 1, -1, 0, 0,
    CHECK_RANGE_EQ (0, N, 1));

  TEST_FUNC (f4, 0, -1, 0, 7,
    CHECK_RANGE_EQ (0, N, 7));

  TEST_FUNC (f5, 1, -1, 0, 4,
    CHECK_RANGE_EQ (0, N, 5));

  TEST_FUNC (f6, 5, -1, 0, 3,
    CHECK_RANGE_EQ (0, N, 8));

  return 0;
}
