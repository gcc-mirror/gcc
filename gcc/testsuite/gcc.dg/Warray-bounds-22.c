/* PR tree-optimization/82588 - missing -Warray-bounds on an excessively
   large index
   { dg-do compile }
   { dg-require-effective-target alloca }
   { dg-options "-O2 -Warray-bounds -ftrack-macro-expansion=0" }  */

#define SIZE_MAX  __SIZE_MAX__
#define DIFF_MAX __PTRDIFF_MAX__
#define DIFF_MIN (-DIFF_MAX - 1)

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__    size_t;

static ptrdiff_t signed_value (void)
{
  extern volatile ptrdiff_t signed_value_source;
  return signed_value_source;
}

static ptrdiff_t signed_range (ptrdiff_t min, ptrdiff_t max)
{
  ptrdiff_t val = signed_value ();
  return val < min || max < val ? min : val;
}

typedef struct AX { int n; char ax[]; } AX;

typedef struct A1 { int i; char a1[1]; } A1;
typedef struct B { int i; struct A1 a1x[]; } B;

void sink (int, ...);

#define T(expr)   sink (0, (expr))

void test_vla (unsigned m, unsigned n)
{
  char vla1[m];

  T (vla1[DIFF_MIN]);                     /* { dg-warning "array subscript -\[0-9\]+ is below array bounds" "vla" } */
  T (vla1[-1]);                           /* { dg-warning "array subscript -1 is below array bounds" "vla" } */
  T (vla1[0]);
  T (vla1[1]);
  T (vla1[m - 1]);
  /* It would be nice to diagnose this. */
  T (vla1[m]);                            /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "bug 82608" { xfail *-*-*} } */
  T (vla1[DIFF_MAX - 1]);
  T (vla1[DIFF_MAX]);                     /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "vla" } */

  ptrdiff_t i = signed_range (DIFF_MAX - 1, DIFF_MAX);
  T (vla1[i]);

  char vla2[m][n];

  T (vla2[0][DIFF_MIN]);                  /* { dg-warning "array subscript -\[0-9\]+ is below array bounds" "vla" } */
  T (vla2[0][-1]);                        /* { dg-warning "array subscript -1 is below array bounds" "vla" } */
  T (vla2[0][0]);
  T (vla2[1][1]);
  T (vla2[m - 1][n - 1]);
  /* It would be nice to diagnose this. */
  T (vla2[m][0]);                         /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "bug 82608" { xfail *-*-*} } */
  T (vla2[m + 1][0]);                     /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "bug 82608" { xfail *-*-*} } */
  T (vla2[0][n]);                         /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "bug 82608" { xfail *-*-*} } */
  T (vla2[0][n + 1]);                     /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "bug 82608" { xfail *-*-*} } */
  T (vla2[m][n]);                         /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "bug 82608" { xfail *-*-*} } */
  T (vla2[m + 1][n + 1]);                 /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "bug 82608" { xfail *-*-*} } */

  T (vla2[0][DIFF_MAX]);                  /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "vla" } */
  T (vla2[DIFF_MAX][0]);                  /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "vla" { xfail *-*-* } } */
  T (vla2[DIFF_MAX][DIFF_MAX]);           /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "vla" } */

  struct S256 { char a[256]; } vla3[m];

  T (vla3[DIFF_MIN].a[0]);                /* { dg-warning "array subscript -\[0-9\]+ is below array bounds" "vla" } */
  T (vla3[-1].a[0]);                      /* { dg-warning "array subscript -1 is below array bounds" "vla" } */
  T (vla3[0].a[0]);
  T (vla3[1].a[0]);
  T (vla3[m - 1].a[0]);
  T (vla3[DIFF_MAX / 256 - 1].a[0]);
  T (vla3[DIFF_MAX / 256].a[0]);          /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "vla" } */

  i = signed_range (DIFF_MAX / 256 - 1, DIFF_MAX);
  T (vla3[i].a[0]);

  i = signed_range (DIFF_MAX / 256, DIFF_MAX);
  T (vla3[i].a[0]);                       /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "vla" } */

  struct VLA { char vla[n]; } x;

  T (x.vla[DIFF_MIN]);                    /* { dg-warning "array subscript -\[0-9\]+ is below array bounds" "vla" } */
  T (x.vla[-1]);                          /* { dg-warning "array subscript -1 is below array bounds" "vla" } */
  T (x.vla[0]);
  T (x.vla[1]);
  T (x.vla[n - 1]);
  T (x.vla[DIFF_MAX - 1]);
  T (x.vla[DIFF_MAX]);                    /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "vla" } */
}
