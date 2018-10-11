/* Verify that calls to strnlen with an unterminated array and
   an excessive non-constant bound are diagnosed.
   { dg-do compile }
   { dg-options "-O2 -Wall -ftrack-macro-expansion=0" } */

#include "range.h"

extern size_t strnlen (const char*, size_t);

const char a[5] = "12345";   /* { dg-message "declared here" } */
enum { asz = sizeof a };

int v0 = 0;
int v1 = 1;

void sink (int, ...);

#define CONCAT(a, b)   a ## b
#define CAT(a, b)      CONCAT(a, b)

#define T(str, n)					\
  __attribute__ ((noipa))				\
  void CAT (test_, __LINE__) (void) {			\
    int i0 = 0, i1 = i0 + 1, i2 = i1 + 1, i3 = i2 + 1;	\
    sink (strnlen (str, n), i0, i1, i2, i3);		\
  } typedef void dummy_type


T (a, UR (asz, -1));
T (a, UR (asz - 1, -1));
T (a, UR (asz - 2, -1));
T (a, UR (asz - 5, -1));
T (&a[0], UR (asz, -1));
T (&a[0] + 1, UR (asz, asz + 1)); /* { dg-warning "specified bound \\\[5, 6] exceeds the size 4 of unterminated array" } */
T (&a[1], UR (asz, 6));           /* { dg-warning "specified bound \\\[5, 6] exceeds the size 4 of unterminated array" } */
T (&a[1], UR (asz - 1, 7));
T (&a[v0], UR (asz, 8));          /* { dg-warning "specified bound \\\[5, 8] may exceed the size of at most 5 of unterminated array" } */
T (&a[v0] + 1, UR (asz, 9));      /* { dg-warning "specified bound \\\[5, 9] may exceed the size of at most 5 of unterminated array" } */

T (a, UR (asz + 1, asz + 2));     /* { dg-warning "specified bound \\\[6, 7] exceeds the size 5 " } */
T (&a[0], UR (asz + 1, 10));      /* { dg-warning "unterminated" } */
T (&a[0] + 1, UR (asz - 1, 11));
T (&a[0] + 1, UR (asz + 1, 12));  /* { dg-warning "unterminated" } */
T (&a[1], UR (asz + 1, 13));      /* { dg-warning "unterminated" } */
T (&a[v0], UR (asz + 1, 14));     /* { dg-warning "unterminated" } */
T (&a[v0] + 1, UR (asz + 1, 15)); /* { dg-warning "unterminated" } */

T (&a[v0] + 1, UR (DIFF_MAX, SIZE_MAX)); /* { dg-warning "unterminated" } */

T (&a[v0] + 1, UR (DIFF_MAX + (size_t)1, SIZE_MAX)); /* { dg-warning "specified bound \\\[\[0-9\]+, \[0-9\]+] exceeds maximum object size " } */


const char c[4] = "1234";

void test (int n0)
{
  char a[] = "123";

  if (n0 < 4)
    n0 = 4;
  int n1 = __builtin_strlen (a);

  int n = n0 < n1 ? n1 : n0;

  sink (strnlen (c + n, n + 1));    /* { dg-warning "specified bound \\\[5, \[0-9\]+] may exceed the size of at most 4 of unterminated array" } */
}
