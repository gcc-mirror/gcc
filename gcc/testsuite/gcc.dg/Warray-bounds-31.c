/* PR tree-optimization/83776: missing -Warray-bounds indexing past the end
   of a string literal
   Test to exercise detection of out-of-bounds indices into narrow string
   literals.
   { dg-do compile }
   { dg-options "-O2 -Warray-bounds -ftrack-macro-expansion=0" } */

#include "range.h"

#define MAX DIFF_MAX
#define MIN DIFF_MIN

#define S1 "1"
#define S3 "123"
#define S7 "1234567"
#define S8 "12345678"
#define S9 "123456789"

void sink (int, ...);

#define T(expr)   sink (0, expr)


void narrow_direct_cst (void)
{
  T (S1[MIN]);                /* { dg-warning "array subscript -\[0-9\]+ is below array bounds of .char\\\[2]" "bug 86611" { xfail lp64 } } */
  T (S1[-1]);                 /* { dg-warning "array subscript -1 is below array bounds of .char\\\[2]" } */
  T (S1[0]);
  T (S1[1]);
  T (S1[2]);                  /* { dg-warning "array subscript 2 is above array bounds of .char\\\[2]" } */
  T (S1[MAX]);                /* { dg-warning "array subscript \[0-9\]+ is above array bounds of .char\\\[2]" } */

  T (&S1[MIN]);               /* { dg-warning "array subscript -\[0-9\]+ is below array bounds of .char\\\[2]" } */
  T (&S1[-1]);                /* { dg-warning "array subscript -1 is below array bounds of .char\\\[2]" } */
  T (&S1[0]);
  T (&S1[2]);
  T (&S1[3]);                 /* { dg-warning "array subscript 3 is above array bounds of .char\\\[2]" } */
  T (&S1[MAX]);               /* { dg-warning "array subscript \[0-9\]+ is above array bounds of .char\\\[2]" } */

  T (S9[MIN]);                /* { dg-warning "array subscript -\[0-9\]+ is below array bounds of .char\\\[10]" "xfail lp64" { xfail lp64 } } */
  T (S9[-1]);                 /* { dg-warning "array subscript -1 is below array bounds of .char\\\[10]" } */
  T (S9[9]);
  T (S9[10]);                 /* { dg-warning "array subscript 10 is above array bounds of .char\\\[10]" } */
  T (S9[11]);                 /* { dg-warning "array subscript 11 is above array bounds of .char\\\[10]" } */
  T (S9[MAX]);                /* { dg-warning "array subscript \[0-9\]+ is above array bounds of .char\\\[10]" } */

  T (&S9[MIN]);               /* { dg-warning "array subscript -\[0-9\]+ is below array bounds of .char\\\[10]" } */
  T (&S9[-1]);                /* { dg-warning "array subscript -1 is below array bounds of .char\\\[10]" } */
  T (&S9[9]);
  T (&S9[10]);
  T (&S9[11]);                 /* { dg-warning "array subscript 11 is above array bounds of .char\\\[10]" } */
  T (&S9[MAX]);               /* { dg-warning "array subscript \[0-9\]+ is above array bounds of .char\\\[10]" } */
}

void narrow_ptr_deref_cst (void)
{
  const char *p = S8 + 9;

  T (*(p + MIN));             /* { dg-warning "array subscript -\[0-9\]+ is outside array bounds of .char\\\[9]." } */
  T (*(p - 10));              /* { dg-warning "array subscript -1 is outside array bounds of .char\\\[9]." } */
  T (*(p - 9));
  T (*(p - 1));
  T (*p);                     /* { dg-warning "array subscript 9 is outside array bounds of .char\\\[9]." } */
  T (*(p + 1));               /* { dg-warning "array subscript 10 is outside array bounds of .char\\\[9]." } */
  T (*(p + 2));               /* { dg-warning "array subscript 11 is outside array bounds of .char\\\[9]." } */
}

void narrow_ptr_index_cst (void)
{
  const char *p = S7;

  T (p[MIN + 1]);             /* { dg-warning "array subscript -\[0-9\]+ is outside array bounds of .char\\\[8]." "bug 86611" { xfail lp64 } } */
  T (p[-1]);                  /* { dg-warning "array subscript -1 is outside array bounds of .char\\\[8]." } */
  T (p[0]);
  T (p[1]);
  T (p[8]);                   /* { dg-warning "array subscript 8 is outside array bounds of .char\\\[8]." } */
  T (p[99]);                  /* { dg-warning "array subscript 99 is outside array bounds of .char\\\[8]." } */
  T (p[MAX]);                 /* { dg-warning "array subscript \[0-9\]+ is outside array bounds of .char\\\[8]." } */

  T (&p[MIN + 1]);            /* { dg-warning "array subscript -\[0-9\]+ is \(below|outside\) array bounds of .char\\\[8]." } */
  T (&p[-1]);                 /* { dg-warning "array subscript -1 is \(below|outside\) array bounds of .char\\\[8]." } */
  T (&p[0]);
  T (&p[1]);
  T (&p[8]);
  T (&p[9]);                  /* { dg-warning "array subscript 9 is \(above|outside\) array bounds of .char\\\[8]." } */
  T (&p[99]);                 /* { dg-warning "array subscript 99 is \(above|outside\) array bounds of .char\\\[8]." } */
  T (&p[MAX]);                /* { dg-warning "array subscript \[0-9\]+ is \(above|outside\) array bounds of .char\\\[8]." } */

  const char *q = S8 + 4;
  T (q[MIN + 1]);             /* { dg-warning "array subscript -?\[0-9\]+ is outside array bounds of .char\\\[9]." "bug 86611" { xfail lp64 } } */
  T (q[-5]);                  /* { dg-warning "array subscript -1 is outside array bounds of .char\\\[9]." } */
  T (q[-4]);
  T (q[0]);
  T (q[1]);
  T (q[3]);
  T (q[4]);
  T (q[5]);                   /* { dg-warning "array subscript 9 is outside array bounds of .char\\\[9]." } */
  T (q[99]);                  /* { dg-warning "array subscript 103 is outside array bounds of .char\\\[9]." } */
  T (q[MAX - 4]);             /* { dg-warning "array subscript \[0-9\]+ is outside array bounds of .char\\\[9]." } */
  T (q[MAX - 3]);             /* { dg-warning "array subscript -?\[0-9\]+ is outside array bounds of .char\\\[9]." "bug 86611" { xfail lp64 } } */

  T (&q[MIN + 1]);            /* { dg-warning "array subscript -?\[0-9\]+ is \(below|outside\) array bounds of .char\\\[9]." } */
  T (&q[-5]);                 /* { dg-warning "array subscript -1 is \(below|outside\) array bounds of .char\\\[9]." } */
  T (&q[-4]);
  T (&q[0]);
  T (&q[1]);
  T (&q[5]);
  T (&q[6]);                  /* { dg-warning "array subscript 10 is \(above|outside\) array bounds of .char\\\[9]." } */
  T (&q[99]);                 /* { dg-warning "array subscript 103 is \(above|outside\) array bounds of .char\\\[9]." } */
  T (&q[MAX - 4]);            /* { dg-warning "array subscript \[0-9\]+ is \(above|outside\) array bounds of .char\\\[9]." } */
  T (&q[MAX - 3]);            /* { dg-warning "array subscript -?\[0-9\]+ is \(below|outside\) array bounds of .char\\\[9]." } */
}


void narrow_direct_range (ptrdiff_t i, size_t j)
{
  T (S3[i]);
  T (S9[j]);

  T (S3[SR (MIN, -1)]);       /* { dg-warning "array subscript -1 is below array bounds of .char\\\[4]" } */
  T (S3[SR (MIN, 0)]);
  T (S3[SR (-2, -1)]);        /* { dg-warning "array subscript -1 is below array bounds of .char\\\[4]" } */
  T (S3[SR (1, 2)]);
  T (S3[SR (1, 999)]);
  T (S3[SR (2, 999)]);
  T (S3[SR (3, 999)]);
  T (S3[SR (4, 999)]);       /* { dg-warning "array subscript 4 is above array bounds of .char\\\[4]" } */

  T (&S3[SR (MIN, -1)]);      /* { dg-warning "array subscript -1 is below array bounds of .char\\\[4]" } */
  T (&S3[SR (MIN, 0)]);
  T (&S3[SR (-2, -1)]);       /* { dg-warning "array subscript -1 is below array bounds of .char\\\[4]" } */
  T (&S3[SR (1, 2)]);
  T (&S3[SR (1, 999)]);
  T (&S3[SR (2, 999)]);
  T (&S3[SR (4, 999)]);
  T (&S3[SR (5, 999)]);      /* { dg-warning "array subscript 5 is above array bounds of .char\\\[4]" } */

  T (S9[SR (MIN, -9)]);       /* { dg-warning "array subscript -9 is below array bounds of .char\\\[10]" } */
  T (S9[SR (MIN, -1)]);       /* { dg-warning "array subscript -1 is below array bounds of .char\\\[10]" } */
  T (S9[SR (MIN, 0)]);
  T (S9[SR (-2, -1)]);        /* { dg-warning "array subscript -1 is below array bounds of .char\\\[10]" } */
  T (S9[SR (1, 2)]);
  T (S9[SR (1, 9)]);
  T (S9[SR (1, 999)]);
  T (S9[SR (9, 999)]);
  T (S9[SR (10, 999)]);       /* { dg-warning "array subscript 10 is above array bounds of .char\\\[10]" } */
  T (S9[SR (99, MAX)]);       /* { dg-warning "array subscript 99 is above array bounds of .char\\\[10]" } */
}


void narrow_ptr_deref_range (ptrdiff_t i, size_t j)
{
  const char *p;

  p = S1 + i;
  T (*p);

  p = S1 + j;
  T (*p);

  p = S1 + SR (-999, 999);
  T (*p);

  p = S1 + SR (-1, 1);
  T (*p);

  p = S1 + SR (-1, 0);
  T (*p);

  p = S1 + SR (0, 1);
  T (*p);

  p = S1 + SR (1, 2);
  T (*p);

  p = S1 + SR (2, 3);
  T (*p);                     /* { dg-warning "array subscript 2 is outside array bounds of .char\\\[2]." } */

  p = S1 + SR (9, 99);
  T (*p);                     /* { dg-warning "array subscript \\\[9, 99] is outside array bounds of .char\\\[2]." } */

  p = S8 + SR (-999, 999);
  T (*p);

  p = S8 + SR (-9, -1);
  T (*p);                     /* { dg-warning "array subscript \\\[-9, -1] is outside array bounds of .char\\\[9]." } */

  p = S8 + SR (-9, 0);
  T (*p);

  p = S8 + SR (-9, 9);
  T (*p);

  p = S8 + SR (-9, 123);
  T (*p);

  p = S8 + SR (8, 123);
  T (*p);

  p = S8 + SR (9, 123);
  T (*p);                     /* { dg-warning "array subscript 9 is outside array bounds of .char\\\[9]." } */

  {
    const char *p1 = S3 + i;
    const char *p2 = p1 + i;
    const char *p3 = p2 + i;
    const char *p4 = p3 + i;
    const char *p5 = p4 + i;

    T (*p1);
    T (*p2);
    T (*p3);
    T (*p4);
    T (*p5);
  }

  {
    i = SR (1, 2);

    const char *p1 = S3 +  SR (1, DIFF_MAX - 1);
    const char *p2 = p1 + i;
    const char *p3 = p2 + i;
    const char *p4 = p3 + i;
    const char *p5 = p4 + i;

    T (*p1);
    T (*p2);
    T (*p3);
    T (*p4);                  /* { dg-warning "array subscript 4 is outside array bounds of .char\\\[4]." } */
    T (*p5);                  /* { dg-warning "array subscript \\\[5, \[0-9\]+] is outside array bounds of .char\\\[4]." } */
  }
}


void narrow_ptr_index_range (void)
{
  const char *p;

  p = S7;
  T (p[SR (-9, -1)]);         /* { dg-warning "array subscript \\\[-9, -1] is outside array bounds of .char\\\[8]." } */
  T (p[SR (-8, 0)]);
  T (p[SR (0, MAX)]);
  T (p[SR (1, 9)]);
  T (p[SR (8, 9)]);           /* { dg-warning "array subscript 8 is outside array bounds of .char\\\[8]." } */

  p = S7 + SR (4, 6);
  T (p[5]);                   /* { dg-warning "array subscript \\\[9, 11] is outside array bounds of .char\\\[8]." } */
}
