/* PR tree-optimization/83776: missing -Warray-bounds indexing past the end
   of a string literal
   Test to exercise warnings for computations of otherwise in-bounds indices
   into strings that temporarily exceed the bounds of the string.
   { dg-do compile }
   { dg-options "-O2 -Warray-bounds=2 -ftrack-macro-expansion=0" } */

#include "range.h"

#define MAX DIFF_MAX
#define MIN DIFF_MIN

typedef __WCHAR_TYPE__ wchar_t;

void sink (int, ...);

#define T(expr)   sink (0, expr)

void test_narrow (void)
{
  int i = SR (1, 2);

  const char *p0 = "12";
  const char *p1 = p0 + i;    /* points at '2' or beyond */
  const char *p2 = p1 + i;    /* points at '\0' or beyond */
  const char *p3 = p2 + i;    /* points just past the end */
  const char *p4 = p3 + i;    /* invalid */

  T (p0[-1]);                 /* { dg-warning "array subscript \(-1|\[0-9\]+) is outside array bounds of .char\\\[3]." } */
  T (p0[0]);
  T (p0[1]);
  T (p0[2]);
  T (p0[3]);                  /* { dg-warning "array subscript 3 is outside array bounds of .char\\\[3]." } */

  T (&p0[-1]);                /* { dg-warning "array subscript \(-1|\[0-9\]+) is \(above|below|outside\) array bounds of .char\\\[3]." } */
  T (&p0[0]);
  T (&p0[1]);
  T (&p0[2]);
  T (&p0[3]);
  T (&p0[4]);                 /* { dg-warning "array subscript 4 is \(above|outside\) array bounds of .char\\\[3]." } */

  T (p1[-3]);                 /* { dg-warning "array subscript \\\[-2, -1] is outside array bounds of .char\\\[3]." } */
  T (p1[-2]);
  T (p1[-1]);
  T (p1[ 0]);
  T (p1[ 1]);
  T (p1[ 2]);                 /* { dg-warning "array subscript \\\[3, 4] is outside array bounds of .char\\\[3]." } */
  T (p1[ 3]);                 /* { dg-warning "array subscript \\\[4, 5] is outside array bounds of .char\\\[3]." } */

  T (&p1[-3]);                /* { dg-warning "array subscript \\\[-2, -1] is outside array bounds of .char\\\[3]." "bug" { xfail *-*-* } } */
  T (&p1[-2]);
  T (&p1[-1]);
  T (&p1[ 0]);
  T (&p1[ 1]);
  T (&p1[ 2]);
  T (&p1[ 3]);                /* { dg-warning "array subscript \\\[4, 6] is outside array bounds of .char\\\[3]." "bug" { xfail *-*-* } } */

  T (p2[-4]);                 /* { dg-warning "intermediate array offset 4 is outside array bounds of .char\\\[3]." } */
  T (p2[-3]);
  T (p2[-2]);
  T (p2[-1]);
  T (p2[ 0]);

  /* Even though the lower bound of p3's offsets is in bounds, in order
     to subtract 4 from p3 and get a dereferenceable pointer its value
     would have to be out-of-bounds.  */
  T (p3[-4]);                 /* { dg-warning "intermediate array offset 4 is outside array bounds of .char\\\[3]." } */
  T (p3[-3]);
  T (p3[-2]);
  T (p3[-1]);
  T (p3[ 0]);                 /* { dg-warning "array subscript \\\[3, 6] is outside array bounds of .char\\\[3]." } */

  T (p4[-4]);                 /* { dg-warning "intermediate array offset 4 is outside array bounds of .char\\\[3]." } */
  T (p4[-3]);                 /* { dg-warning "intermediate array offset 4 is outside array bounds of .char\\\[3]." } */
  T (p4[-2]);                 /* { dg-warning "intermediate array offset 4 is outside array bounds of .char\\\[3]." } */

  /* The final subscripts below are invalid.  */
  T (p4[-1]);                 /* { dg-warning "array subscript \\\[3, 7] is outside array bounds of .char\\\[3]." } */
  T (p4[ 0]);                 /* { dg-warning "array subscript \\\[4, 8] is outside array bounds of .char\\\[3]." } */
}


void test_narrow_vflow (void)
{
  int i = SR (DIFF_MAX - 2, DIFF_MAX);
  int j = SR (1, DIFF_MAX);

  const char *p0 = "123";
  const char *p1 = p0 + i;    /* points at '2' or beyond */
  const char *p2 = p1 + i;    /* points at '\0' or beyond */
  const char *p3 = p2 + i;    /* points just past the end */
  const char *p4 = p3 + i;    /* invalid */
}


void test_wide (void)
{
  int i = SR (1, 2);

  const wchar_t *p0 = L"123";
  const wchar_t *p1 = p0 + i;    /* points at L'2' or beyond */
  const wchar_t *p2 = p1 + i;    /* points at L'3' or beyond */
  const wchar_t *p3 = p2 + i;    /* points at L'\0' or beyond */
  const wchar_t *p4 = p3 + i;    /* points just past the end */
  const wchar_t *p5 = p4 + i;    /* invalid */

  T (p0[0]);
  T (p0[1]);
  T (p0[2]);
  T (p0[3]);
  T (p0[4]);                  /* { dg-warning "array subscript 4 is outside array bounds of .\[a-z \]+\\\[4]." } */

  T (p1[-1]);
  T (p1[ 0]);
  T (p1[ 1]);
  T (p1[ 2]);
  T (p1[ 3]);                  /* { dg-warning "array subscript \\\[4, 5] is outside array bounds of .\[a-z \]+\\\[4]." } */

  T (&p1[-1]);
  T (&p1[ 0]);
  T (&p1[ 1]);
  T (&p1[ 2]);
  T (&p1[ 3]);
  T (&p1[ 4]);                 /* { dg-warning "array subscript \\\[5, 6] is outside array bounds of .\[a-z \]+\\\[4]." "bug" { xfail *-*-* } } */

  T (p2[-5]);                 /* { dg-warning "array subscript \\\[-3, -1] is outside array bounds of .\[a-z \]+\\\[4]." } */
  T (p2[-4]);
  T (p2[-3]);
  T (p2[-2]);
  T (p2[-1]);
  T (p2[ 0]);

  /* Even though the lower bound of p3's offsets is in bounds, in order
     to subtract 5 from p3 and get a dereferenceable pointer its value
     would have to be out-of-bounds.  */
  T (p3[-5]);                 /* { dg-warning "intermediate array offset 5 is outside array bounds of .\[a-z \]+\\\[4]." } */
  T (p3[-4]);
  T (p3[-3]);
  T (p3[-2]);
  T (p3[-1]);
  T (p3[ 0]);
  T (p3[ 1]);                 /* { dg-warning "array subscript \\\[4, 7] is outside array bounds of .\[a-z \]+\\\[4]." } */

  T (p4[-5]);                 /* { dg-warning "intermediate array offset 5 is outside array bounds of .\[a-z \]+\\\[4]." } */
  T (p4[-4]);
  T (p4[-3]);
  T (p4[-2]);
  T (p4[-1]);
  T (p4[ 0]);                 /* { dg-warning "array subscript \\\[4, 8] is outside array bounds of .\[a-z \]+\\\[4]." } */
}
