/* Test for builtin abs, labs, llabs, imaxabs.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */

#include <limits.h>
typedef __INTMAX_TYPE__ intmax_t;
#define INTMAX_MAX __INTMAX_MAX__

extern int abs (int);
extern long labs (long);
extern long long llabs (long long);
extern intmax_t imaxabs (intmax_t);
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
  if (abs (i0) != 0)
    abort ();
  if (abs (0) != 0)
    link_error ();
  if (abs (i1) != 1)
    abort ();
  if (abs (1) != 1)
    link_error ();
  if (abs (im1) != 1)
    abort ();
  if (abs (-1) != 1)
    link_error ();
  if (abs (imin) != INT_MAX)
    abort ();
  if (abs (-INT_MAX) != INT_MAX)
    link_error ();
  if (abs (imax) != INT_MAX)
    abort ();
  if (abs (INT_MAX) != INT_MAX)
    link_error ();
  if (labs (l0) != 0L)
    abort ();
  if (labs (0L) != 0L)
    link_error ();
  if (labs (l1) != 1L)
    abort ();
  if (labs (1L) != 1L)
    link_error ();
  if (labs (lm1) != 1L)
    abort ();
  if (labs (-1L) != 1L)
    link_error ();
  if (labs (lmin) != LONG_MAX)
    abort ();
  if (labs (-LONG_MAX) != LONG_MAX)
    link_error ();
  if (labs (lmax) != LONG_MAX)
    abort ();
  if (labs (LONG_MAX) != LONG_MAX)
    link_error ();
  if (llabs (ll0) != 0LL)
    abort ();
  if (llabs (0LL) != 0LL)
    link_error ();
  if (llabs (ll1) != 1LL)
    abort ();
  if (llabs (1LL) != 1LL)
    link_error ();
  if (llabs (llm1) != 1LL)
    abort ();
  if (llabs (-1LL) != 1LL)
    link_error ();
  if (llabs (llmin) != __LONG_LONG_MAX__)
    abort ();
  if (llabs (-__LONG_LONG_MAX__) != __LONG_LONG_MAX__)
    link_error ();
  if (llabs (llmax) != __LONG_LONG_MAX__)
    abort ();
  if (llabs (__LONG_LONG_MAX__) != __LONG_LONG_MAX__)
    link_error ();
  if (imaxabs (imax0) != 0)
    abort ();
  if (imaxabs (0) != 0)
    link_error ();
  if (imaxabs (imax1) != 1)
    abort ();
  if (imaxabs (1) != 1)
    link_error ();
  if (imaxabs (imaxm1) != 1)
    abort ();
  if (imaxabs (-1) != 1)
    link_error ();
  if (imaxabs (imaxmin) != INTMAX_MAX)
    abort ();
  if (imaxabs (-INTMAX_MAX) != INTMAX_MAX)
    link_error ();
  if (imaxabs (imaxmax) != INTMAX_MAX)
    abort ();
  if (imaxabs (INTMAX_MAX) != INTMAX_MAX)
    link_error ();
}
