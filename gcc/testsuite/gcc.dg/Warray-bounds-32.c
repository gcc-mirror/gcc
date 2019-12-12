/* PR tree-optimization/83776: missing -Warray-bounds indexing past the end
   of a string literal
   Test to exercise indices into wide string literals.
   { dg-do compile }
   { dg-options "-O2 -Warray-bounds -ftrack-macro-expansion=0" } */

#include "range.h"

#define MAX DIFF_MAX
#define MIN DIFF_MIN

typedef __WCHAR_TYPE__ wchar_t;

#define W2 L"12"
#define W3 L"123"
#define W4 L"1234"
#define W7 L"1234567"
#define W8 L"12345678"
#define W9 L"123456789"

void sink (int);

#define T(expr)   sink (expr)


void wide_direct_cst (void)
{
  T (W9[MIN]);                /* { dg-warning "array subscript -\[0-9\]+ is below array bounds of .\[a-z \]+\\\[10]" "bug 86611" { xfail ilp32 } } */
  T (W9[-1]);                 /* { dg-warning "array subscript -1 is below array bounds of .\[a-z \]+\\\[10]" } */
  T (W9[11]);                 /* { dg-warning "array subscript 11 is above array bounds of .\[a-z \]+\\\[10]" } */
  T (W9[MAX]);                /* { dg-warning "array subscript \[0-9\]+ is above array bounds of .\[a-z \]+\\\[10]" } */
}

void wide_ptr_deref_cst (void)
{
  const wchar_t *p = W8 + 9;
  T (*p);                     /* { dg-warning "array subscript 9 is outside array bounds of .\[a-z \]+\\\[9]." } */
  T (p[1]);                   /* { dg-warning "array subscript 10 is outside array bounds of .\[a-z \]+\\\[9]." } */
  T (p[99]);                  /* { dg-warning "array subscript 108 is outside array bounds of .\[a-z \]+\\\[9]." } */
}

void wide_ptr_index_cst (void)
{
  const wchar_t *p = W7;

  T (p[1]);
  T (p[8]);                   /* { dg-warning "array subscript 8 is outside array bounds of .\[a-z \]+\\\[8]." } */
  T (p[99]);                  /* { dg-warning "array subscript 99 is outside array bounds of .\[a-z \]+\\\[8]." } */
  T (p[MAX]);                 /* { dg-warning "array subscript -?\[0-9\]+ is outside array bounds of .\[a-z \]+\\\[8]." } */
}


void wide_direct_range (ptrdiff_t i, size_t j)
{
  T (W9[i]);
  T (W9[j]);

  T (W9[SR (MIN, -9)]);       /* { dg-warning "array subscript -9 is below array bounds of .\[a-z \]+\\\[10]" } */
  T (W9[SR (MIN, -1)]);       /* { dg-warning "array subscript -1 is below array bounds of .\[a-z \]+\\\[10]" } */
  T (W9[SR (MIN, 0)]);
  T (W9[SR (-2, -1)]);        /* { dg-warning "array subscript -1 is below array bounds of .\[a-z \]+\\\[10]" } */
  T (W9[SR (1, 2)]);
  T (W9[SR (1, 9)]);
  T (W9[SR (1, 999)]);
  T (W9[SR (9, 999)]);
  T (W9[SR (10, 999)]);       /* { dg-warning "array subscript 10 is above array bounds of .\[a-z \]+\\\[10]" } */
  T (W9[SR (99, MAX)]);       /* { dg-warning "array subscript 99 is above array bounds of .\[a-z \]+\\\[10]" } */
}

void wide_ptr_deref_range (ptrdiff_t i, size_t j)
{
  const wchar_t *p;

  p = W8 + i;
  T (*p);

  p = W8 + j;
  T (*p);

  p = W8 + SR (-9, -1);
  T (*p);                     /* { dg-warning "array subscript \\\[-9, -1] is outside array bounds of .\[a-z \]+\\\[9]." } */

  p = W8 + SR (-9, 0);
  T (*p);

  p = W8 + SR (-9, 9);
  T (*p);

  p = W8 + SR (9, 123);
  T (*p);                     /* { dg-warning "array subscript \\\[9, 123] is outside array bounds of .\[a-z \]+\\\[9]." } */
}

void wide_ptr_index_range (void)
{
  const wchar_t *p;

  p = W7;
  T (p[SR (-9, -1)]);         /* { dg-warning "array subscript \\\[-9, -1] is outside array bounds of .\[a-z \]+\\\[8]." } */
  T (p[SR (-8, 0)]);
  T (p[SR (0, MAX)]);
  T (p[SR (1, 9)]);
  T (p[SR (8, 9)]);           /* { dg-warning "array subscript \\\[8, 9] is outside array bounds of .\[a-z \]+\\\[8]." } */

  p = W7 + SR (4, 6);
  T (p[5]);                   /* { dg-warning "array subscript \\\[9, 11] is outside array bounds of .\[a-z \]+\\\[8]." } */
}

void wide_ptr_index_range_1 (void)
{
  {
    int i = SR (1, 2);
    const wchar_t *p1 = W2 + i;

    T (p1[0]);
  }
  {
    int i = SR (1, 2);
    const wchar_t *p1 = W2 + i;

    T (p1[1]);
  }
  {
    int i = SR (1, 2);
    const wchar_t *p1 = W2 + i;

    T (p1[2]);                /* { dg-warning "array subscript \\\[3, 4] is outside array bounds of .\[a-z \]+\\\[3]." } */
  }
}

void wide_ptr_index_range_chain (void)
{
  int i = SR (1, 2);
  {
    const wchar_t *p1 = W2 + i;
    const wchar_t *p2 = p1 + i;
    const wchar_t *p3 = p2 + i;

    T (p1[-3]);               /* { dg-warning "array subscript \\\[-2, -1] is outside array bounds of .\[a-z \]+\\\[3]." } */
    T (p1[-2]);
    T (p1[-1]);
    T (p1[0]);
    T (p1[1]);
    T (p1[2]);                /* { dg-warning "array subscript \\\[3, 4] is outside array bounds of .\[a-z \]+\\\[3]." } */

    T (p2[-5]);               /* { dg-warning "array subscript \\\[-3, -1] is outside array bounds of .\[a-z \]+\\\[3]." } */
    T (p2[-4]);
    T (p2[-1]);
    T (p2[0]);
    T (p2[1]);                /* { dg-warning "array subscript \\\[3, 5] is outside array bounds of .\[a-z \]+\\\[3]." } */

    T (p3[0]);                /* { dg-warning "array subscript \\\[3, 6] is outside array bounds of .\[a-z \]+\\\[3]." } */
    T (p3[1]);                /* { dg-warning "array subscript \\\[4, 7] is outside array bounds of .\[a-z \]+\\\[3]." } */
    T (p3[9999]);             /* { dg-warning "array subscript \\\[10002, 10005] is outside array bounds of .\[a-z \]+\\\[3]." } */

    /* Large offsets are indistinguishable from negative values.  */
    T (p3[DIFF_MAX]);         /* { dg-warning "array subscript" "bug" { xfail *-*-* } } */
  }

  {
    const wchar_t *p1 = W3 + i;
    const wchar_t *p2 = p1 + i;
    const wchar_t *p3 = p2 + i;
    const wchar_t *p4 = p3 + i;

    T (p1[-3]);               /* { dg-warning "array subscript \\\[-2, -1] is outside array bounds of .\[a-z \]+\\\[4]." } */
    T (p1[-2]);
    T (p1[1]);
    T (p1[2]);
    T (p1[3]);                /* { dg-warning "array subscript \\\[4, 5] is outside array bounds of .\[a-z \]+\\\[4]." } */

    T (p3[1]);                /* { dg-warning "array subscript \\\[4, 7] is outside array bounds of .\[a-z \]+\\\[4]." } */
  }
}

void wide_ptr_index_range_4 (void)
{
  int i = SR (1, 2);
  const wchar_t *p1 = W4 + i;
  const wchar_t *p2 = p1 + i;
  const wchar_t *p3 = p2 + i;
  const wchar_t *p4 = p3 + i;

  T (p4[1]);                  /* { dg-warning "array subscript \\\[5, 9] is outside array bounds of .\[a-z \]+\\\[5]." } */
}
