/* PR middle-end/81384 - built-in form of strnlen missing

   Verify that a strnlen bound in excess of the maximum object size
   is diagnosed regardless of attribute nonstring.  Also verify that
   a bound that's greater than the size of a non-string array is
   diagnosed, even if it's not in excess of the maximum object size.

   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

#include "range.h"

extern size_t strnlen (const char*, size_t);

#define STR   /* not non-string */
#define NS    __attribute__ ((nonstring))

#define _CAT(s, n)   s ## n
#define CAT(s, n)    _CAT (s, n)
#define UNIQ(n)      CAT (n, __LINE__)

void sink (size_t);

#define T(attr, N, bound)			\
  do {						\
    extern attr char UNIQ (a)[N];		\
    sink (strnlen (UNIQ (a), bound));		\
  } while (0)

void strnlen_cst (void)
{
  size_t n = DIFF_MAX;

  T (STR, /* [] */, n);
  T (STR, /* [] */, n + 1);    /* { dg-warning "specified bound \[0-9\]+ exceeds maximum object size \[0-9\]+" } */

  T (STR, 1, n);
  T (STR, 2, n + 1);           /* { dg-warning "specified bound \[0-9\]+ exceeds maximum object size \[0-9\]+" } */

  T (NS, /* [] */, n);
  T (NS, /* [] */, n + 1);     /* { dg-warning "specified bound \[0-9\]+ exceeds maximum object size \[0-9\]+" } */

  T (NS, 9, n);                /* { dg-warning "argument 1 declared attribute .nonstring. is smaller than the specified bound" } */
  T (NS, 10, n + 1);           /* { dg-warning "specified bound \[0-9\]+ exceeds maximum object size \[0-9\]+" } */
}


void strnlen_range (void)
{
  size_t n = DIFF_MAX;
  n = UR (n, n + 1);

  T (STR, /* [] */, n);
  T (STR, /* [] */, n + 1);    /* { dg-warning "specified bound \\\[\[0-9\]+, \[0-9\]+] exceeds maximum object size \[0-9\]+" } */

  T (STR, 1, n);
  T (STR, 2, n + 1);           /* { dg-warning "specified bound \\\[\[0-9\]+, \[0-9\]+] exceeds maximum object size \[0-9\]+" } */

  T (NS, /* [] */, n);
  T (NS, /* [] */, n + 1);     /* { dg-warning "specified bound \\\[\[0-9\]+, \[0-9\]+] exceeds maximum object size \[0-9\]+" } */

  T (NS, 9, n);                /* { dg-warning "argument 1 declared attribute .nonstring. is smaller than the specified bound" } */
  T (NS, 10, n + 1);           /* { dg-warning "specified bound \\\[\[0-9\]+, \[0-9\]+] exceeds maximum object size \[0-9\]+" } */
}
