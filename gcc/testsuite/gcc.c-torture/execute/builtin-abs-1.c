/* Test for builtin abs, labs, llabs, imaxabs.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */

/* These next definitions are kludges.  When GCC has a <stdint.h> it
   should be used.
*/
#include <limits.h>
#if INT_MAX == __LONG_LONG_MAX__
typedef int intmax_t;
#define INTMAX_MAX INT_MAX
#elif LONG_MAX == __LONG_LONG_MAX__
typedef long intmax_t;
#define INTMAX_MAX LONG_MAX
#else
typedef long long intmax_t;
#define INTMAX_MAX __LONG_LONG_MAX__
#endif

extern int abs (int);
extern long labs (long);
extern long long llabs (long long);
extern intmax_t imaxabs (intmax_t);
extern void abort (void);
extern void exit (int);

extern void link_failure (void);

int
main (void)
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
    link_failure ();
  if (abs (i1) != 1)
    abort ();
  if (abs (1) != 1)
    link_failure ();
  if (abs (im1) != 1)
    abort ();
  if (abs (-1) != 1)
    link_failure ();
  if (abs (imin) != INT_MAX)
    abort ();
  if (abs (-INT_MAX) != INT_MAX)
    link_failure ();
  if (abs (imax) != INT_MAX)
    abort ();
  if (abs (INT_MAX) != INT_MAX)
    link_failure ();
  if (labs (l0) != 0L)
    abort ();
  if (labs (0L) != 0L)
    link_failure ();
  if (labs (l1) != 1L)
    abort ();
  if (labs (1L) != 1L)
    link_failure ();
  if (labs (lm1) != 1L)
    abort ();
  if (labs (-1L) != 1L)
    link_failure ();
  if (labs (lmin) != LONG_MAX)
    abort ();
  if (labs (-LONG_MAX) != LONG_MAX)
    link_failure ();
  if (labs (lmax) != LONG_MAX)
    abort ();
  if (labs (LONG_MAX) != LONG_MAX)
    link_failure ();
  if (llabs (ll0) != 0LL)
    abort ();
  if (llabs (0LL) != 0LL)
    link_failure ();
  if (llabs (ll1) != 1LL)
    abort ();
  if (llabs (1LL) != 1LL)
    link_failure ();
  if (llabs (llm1) != 1LL)
    abort ();
  if (llabs (-1LL) != 1LL)
    link_failure ();
  if (llabs (llmin) != __LONG_LONG_MAX__)
    abort ();
  if (llabs (-__LONG_LONG_MAX__) != __LONG_LONG_MAX__)
    link_failure ();
  if (llabs (llmax) != __LONG_LONG_MAX__)
    abort ();
  if (llabs (__LONG_LONG_MAX__) != __LONG_LONG_MAX__)
    link_failure ();
  if (imaxabs (imax0) != 0)
    abort ();
  if (imaxabs (0) != 0)
    link_failure ();
  if (imaxabs (imax1) != 1)
    abort ();
  if (imaxabs (1) != 1)
    link_failure ();
  if (imaxabs (imaxm1) != 1)
    abort ();
  if (imaxabs (-1) != 1)
    link_failure ();
  if (imaxabs (imaxmin) != INTMAX_MAX)
    abort ();
  if (imaxabs (-INTMAX_MAX) != INTMAX_MAX)
    link_failure ();
  if (imaxabs (imaxmax) != INTMAX_MAX)
    abort ();
  if (imaxabs (INTMAX_MAX) != INTMAX_MAX)
    link_failure ();
  exit (0);
}

/* All the above cases should have been optimized to something else,
   even if not optimizing (unless -fno-builtin was specified).  So any
   remaining calls to the original functions should abort.  */

static int
abs (int x)
{
  abort ();
}

static long
labs (long x)
{
  abort ();
}

static long long
llabs (long long x)
{
  abort ();
}

static intmax_t
imaxabs (intmax_t x)
{
  abort ();
}

/* When optimizing, all the constant cases should have been
   constant folded, so no calls to link_failure should remain.  In any case,
   link_failure should not be called.  */

#ifndef __OPTIMIZE__
void
link_failure (void)
{
  abort ();
}
#endif
