/* PR middle-end/81384 - built-in form of strnlen missing
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

#include "range.h"

extern void* memcpy (void*, const void*, size_t);
extern size_t strnlen (const char*, size_t);

#define NONSTRING __attribute__ ((nonstring))

#define _CAT(s, n)   s ## n
#define CAT(s, n)    _CAT (s, n)
#define UNIQ(n)      CAT (n, __LINE__)

void sink (size_t, ...);

#define T(expr)   sink (expr)

void test_strnlen_array_cst (void)
{
  NONSTRING char ns3[3];
  sink (0, ns3);    // "initialize" ns3

  T (strnlen (ns3, 0));
  T (strnlen (ns3, 1));
  T (strnlen (ns3, 2));
  T (strnlen (ns3, 3));
  T (strnlen (ns3, 4));             /* { dg-warning "argument 1 declared attribute .nonstring. is smaller than the specified bound 4" } */
  T (strnlen (ns3, DIFF_MAX));      /* { dg-warning "argument 1 declared attribute .nonstring. is smaller than the specified bound \[0-9\]+" } */
  T (strnlen (ns3, SIZE_MAX));      /* { dg-warning "specified bound \[0-9\]+ exceeds maximum object size \[0-9\]+" } */

  NONSTRING char ns5[5];
  sink (0, ns5);

  T (strnlen (ns5, 0));
  T (strnlen (ns5, 1));
  T (strnlen (ns5, 2));
  T (strnlen (ns5, 3));
  T (strnlen (ns5, 6));             /* { dg-warning "argument 1 declared attribute .nonstring. is smaller than the specified bound 6" } */
  T (strnlen (ns5, DIFF_MAX));      /* { dg-warning "argument 1 declared attribute .nonstring. is smaller than the specified bound \[0-9\]+" } */
  T (strnlen (ns5, SIZE_MAX));      /* { dg-warning "specified bound \[0-9\]+ exceeds maximum object size \[0-9\]+" } */
}


void test_strnlen_array_range (void)
{
  NONSTRING char ns3[3];
  sink (0, ns3);    // "initialize" ns3

  T (strnlen (ns3, UR (0, 3)));
  T (strnlen (ns3, UR (0, 9)));
  T (strnlen (ns3, UR (3, 4)));
  T (strnlen (ns3, UR (3, DIFF_MAX)));
  T (strnlen (ns3, UR (4, 5)));     /* { dg-warning "argument 1 declared attribute .nonstring. is smaller than the specified bound \\\[4, 5]" } */
  T (strnlen (ns3, UR (DIFF_MAX, SIZE_MAX)));  /* { dg-warning "argument 1 declared attribute .nonstring. is smaller " } */
}


#undef T
#define T(N, init, nelts, bound)			\
  do {							\
    extern NONSTRING char UNIQ (arr)[N];		\
    memcpy (UNIQ (arr), init, nelts);			\
    sink (strnlen (UNIQ (arr), bound), UNIQ (arr));	\
  } while (0)

void test_strnlen_string_cst (void)
{
  T (3, "1",   2, 1);
  T (3, "1",   2, 2);
  T (3, "1",   2, 3);
  T (3, "12",  3, 1);
  T (3, "12",  3, 9);
  T (3, "123", 3, 1);
  T (3, "123", 3, 4);               /* { dg-warning "argument 1 declared attribute .nonstring. is smaller than the specified bound 4" } */
  T (3, "123", 3, 9);               /* { dg-warning "argument 1 declared attribute .nonstring. is smaller than the specified bound 9" } */

  T (5, "1",   2, 1);
  T (5, "1",   2, 2);
  T (5, "1",   2, 9);

  T (5, "12",  3, 1);
  T (5, "12",  3, 9);
  T (5, "123", 3, 1);
  T (5, "123", 3, 5);
  T (5, "123", 3, 6);               /* { dg-warning "argument 1 declared attribute .nonstring. is smaller than the specified bound 6" } */

  /* Strnlen shouldn't trigger a warning for arrays of unknown size
     (except for accesses to uninitialized elements when those are
     detected).  */
  T (/* [] */, "1", 1, 1);
  T (/* [] */, "1", 1, 2);
  T (/* [] */, "1", 2, 1);
  T (/* [] */, "1", 2, 2);
  T (/* [] */, "1", 2, 3);
  T (/* [] */, "1", 2, 9);
  T (/* [] */, "1", 2, DIFF_MAX);
  T (/* [] */, "1", 2, SIZE_MAX);

  size_t n = DIFF_MAX;
  T (/* [] */, "123", 3, n);
  T (/* [] */, "123", 3, n + 1);    /* { dg-warning "specified bound \[0-9\]+ exceeds maximum object size " } */
  n = SIZE_MAX;
  T (/* [] */, "123", 3, n);        /* { dg-warning "specified bound \[0-9\]+ exceeds maximum object size " } */
}


void test_strnlen_string_range (void)
{
  T (3, "1",   2, UR (0, 1));
  T (3, "1",   2, UR (3, 9));
  T (3, "123", 3, UR (4, 5));       /* { dg-warning "argument 1 declared attribute .nonstring. is smaller than the specified bound \\\[4, 5]" } */
  T (3, "123", 3, UR (5, 9));       /* { dg-warning "argument 1 declared attribute .nonstring. is smaller than the specified bound \\\[5, 9]" } */
}
