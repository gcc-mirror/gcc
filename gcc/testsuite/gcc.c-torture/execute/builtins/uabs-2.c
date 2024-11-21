/* Test for builtin uabs, ulabs, ullabs, uimaxabs.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */

#include <limits.h>
typedef __INTMAX_TYPE__ intmax_t;
typedef unsigned __INTMAX_TYPE__ uintmax_t;
#define INTMAX_MAX __INTMAX_MAX__

extern unsigned int uabs (int);
extern unsigned long ulabs (long);
extern unsigned long long ullabs (long long);
extern uintmax_t uimaxabs (intmax_t);
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
  if (uabs (i0) != 0)
    abort ();
  if (uabs (0) != 0)
    link_error ();
  if (uabs (i1) != 1)
    abort ();
  if (uabs (1) != 1)
    link_error ();
  if (uabs (im1) != 1)
    abort ();
  if (uabs (-1) != 1)
    link_error ();
  if (uabs (imin) != INT_MAX)
    abort ();
  if (uabs (imin - 1) != 1U + INT_MAX)
    abort ();
  if (uabs (-INT_MAX) != INT_MAX)
    link_error ();
  if (uabs (-INT_MAX - 1) != 1U + INT_MAX)
    link_error ();
  if (uabs (imax) != INT_MAX)
    abort ();
  if (uabs (INT_MAX) != INT_MAX)
    link_error ();
  if (ulabs (l0) != 0L)
    abort ();
  if (ulabs (0L) != 0L)
    link_error ();
  if (ulabs (l1) != 1L)
    abort ();
  if (ulabs (1L) != 1L)
    link_error ();
  if (ulabs (lm1) != 1L)
    abort ();
  if (ulabs (-1L) != 1L)
    link_error ();
  if (ulabs (lmin) != LONG_MAX)
    abort ();
  if (ulabs (lmin - 1) != 1UL + LONG_MAX)
    abort ();
  if (ulabs (-LONG_MAX) != LONG_MAX)
    link_error ();
  if (ulabs (-LONG_MAX - 1) != 1UL + LONG_MAX)
    link_error ();
  if (ulabs (lmax) != LONG_MAX)
    abort ();
  if (ulabs (LONG_MAX) != LONG_MAX)
    link_error ();
  if (ullabs (ll0) != 0LL)
    abort ();
  if (ullabs (0LL) != 0LL)
    link_error ();
  if (ullabs (ll1) != 1LL)
    abort ();
  if (ullabs (1LL) != 1LL)
    link_error ();
  if (ullabs (llm1) != 1LL)
    abort ();
  if (ullabs (-1LL) != 1LL)
    link_error ();
  if (ullabs (llmin) != __LONG_LONG_MAX__)
    abort ();
  if (ullabs (llmin - 1) != 1ULL + __LONG_LONG_MAX__)
    abort ();
  if (ullabs (-__LONG_LONG_MAX__) != __LONG_LONG_MAX__)
    link_error ();
  if (ullabs (-__LONG_LONG_MAX__ - 1) != 1ULL + __LONG_LONG_MAX__)
    link_error ();
  if (ullabs (llmax) != __LONG_LONG_MAX__)
    abort ();
  if (ullabs (__LONG_LONG_MAX__) != __LONG_LONG_MAX__)
    link_error ();
  if (uimaxabs (imax0) != 0)
    abort ();
  if (uimaxabs (0) != 0)
    link_error ();
  if (uimaxabs (imax1) != 1)
    abort ();
  if (uimaxabs (1) != 1)
    link_error ();
  if (uimaxabs (imaxm1) != 1)
    abort ();
  if (uimaxabs (-1) != 1)
    link_error ();
  if (uimaxabs (imaxmin) != INTMAX_MAX)
    abort ();
  if (uimaxabs (imaxmin - 1) != (uintmax_t) 1 + INTMAX_MAX)
    abort ();
  if (uimaxabs (-INTMAX_MAX) != INTMAX_MAX)
    link_error ();
  if (uimaxabs (-INTMAX_MAX - 1) != (uintmax_t) 1 + INTMAX_MAX)
    link_error ();
  if (uimaxabs (imaxmax) != INTMAX_MAX)
    abort ();
  if (uimaxabs (INTMAX_MAX) != INTMAX_MAX)
    link_error ();
}
