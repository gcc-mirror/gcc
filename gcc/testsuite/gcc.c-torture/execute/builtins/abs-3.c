/* Test for builtin abs, labs, llabs, imaxabs.  Test for __builtin versions. */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */

#include <limits.h>
typedef __INTMAX_TYPE__ intmax_t;
#define INTMAX_MAX __INTMAX_MAX__

extern void abort (void);
extern void link_error (void);

void
main_test (void)
{
  /* For each type, test both runtime and compile time (constant folding)
     optimization.  */
  volatile int i0 = 0, i1 = 1, im1 = -1, imin = -INT_MAX, imax = INT_MAX;
  volatile long l0 = 0L, l1 = 1L, lm1 = -1L, lmin = -LONG_MAX, lmax = LONG_MAX;
  volatile long long ll0 = 0LL, ll1 = 1LL, llm1 = -1LL;
  volatile long long llmin = -__LONG_LONG_MAX__, llmax = __LONG_LONG_MAX__;
  volatile intmax_t imax0 = 0, imax1 = 1, imaxm1 = -1;
  volatile intmax_t imaxmin = -INTMAX_MAX, imaxmax = INTMAX_MAX;
  if (__builtin_abs (i0) != 0)
    abort ();
  if (__builtin_abs (0) != 0)
    link_error ();
  if (__builtin_abs (i1) != 1)
    abort ();
  if (__builtin_abs (1) != 1)
    link_error ();
  if (__builtin_abs (im1) != 1)
    abort ();
  if (__builtin_abs (-1) != 1)
    link_error ();
  if (__builtin_abs (imin) != INT_MAX)
    abort ();
  if (__builtin_abs (-INT_MAX) != INT_MAX)
    link_error ();
  if (__builtin_abs (imax) != INT_MAX)
    abort ();
  if (__builtin_abs (INT_MAX) != INT_MAX)
    link_error ();
  if (__builtin_labs (l0) != 0L)
    abort ();
  if (__builtin_labs (0L) != 0L)
    link_error ();
  if (__builtin_labs (l1) != 1L)
    abort ();
  if (__builtin_labs (1L) != 1L)
    link_error ();
  if (__builtin_labs (lm1) != 1L)
    abort ();
  if (__builtin_labs (-1L) != 1L)
    link_error ();
  if (__builtin_labs (lmin) != LONG_MAX)
    abort ();
  if (__builtin_labs (-LONG_MAX) != LONG_MAX)
    link_error ();
  if (__builtin_labs (lmax) != LONG_MAX)
    abort ();
  if (__builtin_labs (LONG_MAX) != LONG_MAX)
    link_error ();
  if (__builtin_llabs (ll0) != 0LL)
    abort ();
  if (__builtin_llabs (0LL) != 0LL)
    link_error ();
  if (__builtin_llabs (ll1) != 1LL)
    abort ();
  if (__builtin_llabs (1LL) != 1LL)
    link_error ();
  if (__builtin_llabs (llm1) != 1LL)
    abort ();
  if (__builtin_llabs (-1LL) != 1LL)
    link_error ();
  if (__builtin_llabs (llmin) != __LONG_LONG_MAX__)
    abort ();
  if (__builtin_llabs (-__LONG_LONG_MAX__) != __LONG_LONG_MAX__)
    link_error ();
  if (__builtin_llabs (llmax) != __LONG_LONG_MAX__)
    abort ();
  if (__builtin_llabs (__LONG_LONG_MAX__) != __LONG_LONG_MAX__)
    link_error ();
  if (__builtin_imaxabs (imax0) != 0)
    abort ();
  if (__builtin_imaxabs (0) != 0)
    link_error ();
  if (__builtin_imaxabs (imax1) != 1)
    abort ();
  if (__builtin_imaxabs (1) != 1)
    link_error ();
  if (__builtin_imaxabs (imaxm1) != 1)
    abort ();
  if (__builtin_imaxabs (-1) != 1)
    link_error ();
  if (__builtin_imaxabs (imaxmin) != INTMAX_MAX)
    abort ();
  if (__builtin_imaxabs (-INTMAX_MAX) != INTMAX_MAX)
    link_error ();
  if (__builtin_imaxabs (imaxmax) != INTMAX_MAX)
    abort ();
  if (__builtin_imaxabs (INTMAX_MAX) != INTMAX_MAX)
    link_error ();
}
