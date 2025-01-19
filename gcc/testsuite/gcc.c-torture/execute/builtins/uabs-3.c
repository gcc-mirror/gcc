/* Test for builtin uabs, ulabs, ullabs, uimaxabs.  Test for __builtin versions. */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */

#include <limits.h>
typedef __INTMAX_TYPE__ intmax_t;
typedef unsigned __INTMAX_TYPE__ uintmax_t;
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
  if (__builtin_uabs (i0) != 0)
    abort ();
  if (__builtin_uabs (0) != 0)
    link_error ();
  if (__builtin_uabs (i1) != 1)
    abort ();
  if (__builtin_uabs (1) != 1)
    link_error ();
  if (__builtin_uabs (im1) != 1)
    abort ();
  if (__builtin_uabs (-1) != 1)
    link_error ();
  if (__builtin_uabs (imin) != INT_MAX)
    abort ();
  if (__builtin_uabs (imin - 1) != 1U + INT_MAX)
    abort ();
  if (__builtin_uabs (-INT_MAX) != INT_MAX)
    link_error ();
  if (__builtin_uabs (-INT_MAX - 1) != 1U + INT_MAX)
    link_error ();
  if (__builtin_uabs (imax) != INT_MAX)
    abort ();
  if (__builtin_uabs (INT_MAX) != INT_MAX)
    link_error ();
  if (__builtin_ulabs (l0) != 0L)
    abort ();
  if (__builtin_ulabs (0L) != 0L)
    link_error ();
  if (__builtin_ulabs (l1) != 1L)
    abort ();
  if (__builtin_ulabs (1L) != 1L)
    link_error ();
  if (__builtin_ulabs (lm1) != 1L)
    abort ();
  if (__builtin_ulabs (-1L) != 1L)
    link_error ();
  if (__builtin_ulabs (lmin) != LONG_MAX)
    abort ();
  if (__builtin_ulabs (lmin - 1) != 1UL + LONG_MAX)
    abort ();
  if (__builtin_ulabs (-LONG_MAX) != LONG_MAX)
    link_error ();
  if (__builtin_ulabs (-LONG_MAX - 1) != 1UL + LONG_MAX)
    link_error ();
  if (__builtin_ulabs (lmax) != LONG_MAX)
    abort ();
  if (__builtin_ulabs (LONG_MAX) != LONG_MAX)
    link_error ();
  if (__builtin_ullabs (ll0) != 0LL)
    abort ();
  if (__builtin_ullabs (0LL) != 0LL)
    link_error ();
  if (__builtin_ullabs (ll1) != 1LL)
    abort ();
  if (__builtin_ullabs (1LL) != 1LL)
    link_error ();
  if (__builtin_ullabs (llm1) != 1LL)
    abort ();
  if (__builtin_ullabs (-1LL) != 1LL)
    link_error ();
  if (__builtin_ullabs (llmin) != __LONG_LONG_MAX__)
    abort ();
  if (__builtin_ullabs (llmin - 1) != 1ULL + __LONG_LONG_MAX__)
    abort ();
  if (__builtin_ullabs (-__LONG_LONG_MAX__) != __LONG_LONG_MAX__)
    link_error ();
  if (__builtin_ullabs (-__LONG_LONG_MAX__ - 1) != 1ULL + __LONG_LONG_MAX__)
    link_error ();
  if (__builtin_ullabs (llmax) != __LONG_LONG_MAX__)
    abort ();
  if (__builtin_ullabs (__LONG_LONG_MAX__) != __LONG_LONG_MAX__)
    link_error ();
  if (__builtin_uimaxabs (imax0) != 0)
    abort ();
  if (__builtin_uimaxabs (0) != 0)
    link_error ();
  if (__builtin_uimaxabs (imax1) != 1)
    abort ();
  if (__builtin_uimaxabs (1) != 1)
    link_error ();
  if (__builtin_uimaxabs (imaxm1) != 1)
    abort ();
  if (__builtin_uimaxabs (-1) != 1)
    link_error ();
  if (__builtin_uimaxabs (imaxmin) != INTMAX_MAX)
    abort ();
  if (__builtin_uimaxabs (imaxmin - 1) != (uintmax_t) 1 + INTMAX_MAX)
    abort ();
  if (__builtin_uimaxabs (-INTMAX_MAX) != INTMAX_MAX)
    link_error ();
  if (__builtin_uimaxabs (-INTMAX_MAX - 1) != (uintmax_t) 1 + INTMAX_MAX)
    link_error ();
  if (__builtin_uimaxabs (imaxmax) != INTMAX_MAX)
    abort ();
  if (__builtin_uimaxabs (INTMAX_MAX) != INTMAX_MAX)
    link_error ();
}
