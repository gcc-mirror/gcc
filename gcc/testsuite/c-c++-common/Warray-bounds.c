/* PR tree-optimization/82588 - missing -Warray-bounds on an excessively
   large index
   { dg-do compile }
   { dg-require-effective-target alloca }
   { dg-options "-O2 -Warray-bounds -Wno-stringop-overread -ftrack-macro-expansion=0" }  */

#include "../gcc.dg/range.h"

#define offsetof(T, m)   __builtin_offsetof (T, m)

typedef struct AX { int n; char ax[]; } AX;

typedef struct A1 { int i; char a1[1]; } A1;
typedef struct B { int i; struct A1 a1x[]; } B;

void sink (int, ...);

#define R(min, max) signed_range (min, max)
#define T(expr)     sink (0, expr)

struct __attribute__ ((packed)) S16 { unsigned i: 16; };

void farr_char (void)
{
  extern char ac[];

  T (ac[DIFF_MIN]);                       /* { dg-warning "array subscript -\[0-9\]+ is below array bounds of .char *\\\[]." } */
  T (ac[-1]);                             /* { dg-warning "array subscript -1 is below array bounds" } */
  T (ac[0]);

  T (ac[DIFF_MAX - 1]);
  T (ac[DIFF_MAX]);                       /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (ac[DIFF_MAX + (size_t)1]);           /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (ac[SIZE_MAX]);                       /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (ac[R (DIFF_MAX - 1, DIFF_MAX)]);
  T (ac[R (DIFF_MIN + 1, -1)]);           /* { dg-warning "array subscript -1 is below array bounds" } */
  T (ac[R (DIFF_MIN + 1, 0)]);
  T (ac[R (-1, 0)]);
  T (ac[R (-1, 1)]);
}

void farr_s16 (void)
{
  extern struct S16 ax[];

  T (ax[DIFF_MIN]);                       /* { dg-warning "array subscript -\[0-9\]+ is below array bounds of .(struct )?S16 *\\\[]." } */
  T (ax[-1]);                             /* { dg-warning "array subscript -1 is below array bounds" } */
  T (ax[0]);

  T (ax[DIFF_MAX / 2 - 1]);               /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "llp64" { target llp64 } } */
  T (ax[DIFF_MAX / 2]);                   /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (ax[DIFF_MAX / 2 + (size_t)1]);       /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (ax[SIZE_MAX]);                       /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (ax[R (DIFF_MIN, -1)]);               /* { dg-warning "array subscript -1 is below array bounds" } */
  T (ax[R (DIFF_MAX / 2 - 1, DIFF_MAX)]); /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "llp64" { target llp64 } } */
  T (ax[R (DIFF_MAX / 2, DIFF_MAX)]);     /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
}

void farr_s16_7 (void)
{
  extern struct S16 ax_7[][7];

  T (ax_7[0][DIFF_MIN]);                  /* { dg-warning "array subscript -\[0-9\]+ is below array bounds of .(struct )?S16 *\\\[7]." } */
  T (ax_7[0][-1]);                        /* { dg-warning "array subscript -1 is below array bounds" } */
  T (ax_7[0][0]);
  T (ax_7[0][7]);                         /* { dg-warning "array subscript 7 is above array bounds of .(struct )?S16 *\\\[7]." } */
  T (ax_7[0][8]);                         /* { dg-warning "array subscript 8 is above array bounds of .(struct )?S16 *\\\[7]." } */

  T (ax_7[0][DIFF_MAX / 2]);             /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (ax_7[0][SIZE_MAX]);                 /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */

  T (ax_7[DIFF_MIN][0]);                 /* { dg-warning "array subscript -\[0-9\]+ is below array bounds of .(struct )?S16 *\\\[]\\\[7]." } */
  T (ax_7[-1][0]);                        /* { dg-warning "array subscript -1 is below array bounds" } */

  T (ax_7[DIFF_MAX / 2][0]);              /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (ax_7[SIZE_MAX][0]);                  /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */

  ptrdiff_t i = R (DIFF_MIN, -1);
  T (ax_7[i][0]);                         /* { dg-warning "array subscript -1 is below array bounds" } */

  T (ax_7[R (DIFF_MIN, -1)][0]);          /* { dg-warning "array subscript -1 is below array bounds" } */
  T (ax_7[R (DIFF_MIN, 0)][0]);
  T (ax_7[R (-2, -1)][0]);                /* { dg-warning "array subscript -1 is below array bounds" } */
  T (ax_7[R (-1, 0)][0]);
  T (ax_7[R (-1, 1)][0]);
  T (ax_7[R (-1, 7)][0]);
  T (ax_7[R (-1, DIFF_MAX)][0]);

  T (ax_7[R ( 1, DIFF_MAX)][0]);
  T (ax_7[R (DIFF_MAX / 14 - 1, DIFF_MAX)][0]); /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "llp64" { target llp64 } } */

  i = R (DIFF_MAX / 14, DIFF_MAX);
  T (ax_7[i][0]);                         /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */

  T (ax_7[0][R (DIFF_MIN, 0)]);
  T (ax_7[0][R (-1, 0)]);
  T (ax_7[0][R (-1, 1)]);
  T (ax_7[0][R (-1, 7)]);
  T (ax_7[0][R (-1, DIFF_MAX)]);
  T (ax_7[0][R (-1, DIFF_MAX)]);

  T (ax_7[0][R (1, DIFF_MAX)]);
  T (ax_7[0][R (7, DIFF_MAX)]);           /* { dg-warning "array subscript 7 is above array bounds" } */

}

void farr_x_5_7 (void)
{
  extern struct S16 a[][5][7];

  T (a[0][0][-3]);                        /* { dg-warning "array subscript -3 is below array bounds of .(struct )?S16 *\\\[7]." } */
  T (a[0][-2][0]);                        /* { dg-warning "array subscript -2 is below array bounds of .(struct )?S16 *\\\[5]\\\[7]." } */
  T (a[-1][0][0]);                        /* { dg-warning "array subscript -1 is below array bounds of .(struct )?S16 *\\\[]\\\[5]\\\[7]." } */
  T (a[R (-4, -3)][0][0]);                /* { dg-warning "array subscript -3 is below array bounds" } */
  T (a[0][R (-3, -2)][0]);                /* { dg-warning "array subscript -2 is below array bounds" } */
  T (a[0][0][R (-2, -1)]);                /* { dg-warning "array subscript -1 is below array bounds" } */
}


void fax (struct AX *p)
{
  T (p->ax[DIFF_MIN]);                   /* { dg-warning "array subscript -\[0-9\]+ is below array bounds" } */
  T (p->ax[-1]);                          /* { dg-warning "array subscript -1 is below array bounds" } */
  T (p->ax[0]);
  T (p->ax[DIFF_MAX - sizeof *p - 1]);
  T (p->ax[DIFF_MAX - sizeof *p]);        /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (p->ax[DIFF_MAX - sizeof *p + 1]);    /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (p->ax[SIZE_MAX]);                    /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (p->ax[R (DIFF_MIN, -1)]);            /* { dg-warning "array subscript -1 is below array bounds" } */
  T (p->ax[R (-1, 1)]);
  T (p->ax[R (0, DIFF_MAX - 1)]);
  T (p->ax[R (DIFF_MAX - 1, DIFF_MAX)]);/* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
}

void fa1 (struct A1 *p)
{
  T (p->a1[DIFF_MIN]);                    /* { dg-warning "array subscript -\[0-9\]+ is below array bounds" } */
  T (p->a1[-1]);                          /* { dg-warning "array subscript -1 is below array bounds" } */
  T (p->a1[0]);
  T (p->a1[9]);
  T (p->a1[DIFF_MAX - offsetof (A1, a1) - 1]);
  T (p->a1[DIFF_MAX - offsetof (A1, a1)]);/* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (p->a1[DIFF_MAX - offsetof (A1, a1) + 1]); /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (p->a1[SIZE_MAX]);                    /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
}

void fb (struct B *p)
{
  T (p->a1x->a1[DIFF_MIN]);               /* { dg-warning "array subscript -\[0-9\]+ is below array bounds" } */
  T (p->a1x->a1[-1]);                     /* { dg-warning "array subscript -1 is below array bounds" } */
  T (p->a1x->a1[0]);
  T (p->a1x->a1[9]);                      /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (p->a1x->a1[DIFF_MAX - sizeof *p]);   /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (p->a1x->a1[DIFF_MAX - sizeof *p + 1]); /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (p->a1x->a1[SIZE_MAX]);               /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */

  T (p->a1x[1].a1[DIFF_MIN]);             /* { dg-warning "array subscript -\[0-9\]+ is below array bounds" } */
  T (p->a1x[1].a1[-1]);                   /* { dg-warning "array subscript -1 is below array bounds" } */
  T (p->a1x[1].a1[0]);
  T (p->a1x[1].a1[9]);                    /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (p->a1x[1].a1[DIFF_MAX - sizeof *p]); /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (p->a1x[1].a1[DIFF_MAX - sizeof *p + 1]); /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (p->a1x[1].a1[SIZE_MAX]);             /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */

  T (p->a1x[2].a1[DIFF_MIN]);             /* { dg-warning "array subscript -\[0-9\]+ is below array bounds" } */
  T (p->a1x[2].a1[-1]);                   /* { dg-warning "array subscript -1 is below array bounds" } */
  T (p->a1x[2].a1[0]);
  T (p->a1x[2].a1[9]);                    /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (p->a1x[2].a1[DIFF_MAX - sizeof *p]);/* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (p->a1x[2].a1[DIFF_MAX - sizeof *p + 1]); /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (p->a1x[2].a1[SIZE_MAX]);             /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */

  T (p->a1x[3].a1[DIFF_MIN]);             /* { dg-warning "array subscript -\[0-9\]+ is below array bounds" } */
  T (p->a1x[3].a1[-1]);                   /* { dg-warning "array subscript -1 is below array bounds" } */
  T (p->a1x[3].a1[0]);
  T (p->a1x[3].a1[9]);                    /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */

  T (p->a1x[9].a1[0]);

  enum { MAX = (DIFF_MAX - sizeof *p) / sizeof *p->a1x };

  T (p->a1x[DIFF_MIN].a1);                /* { dg-warning "array subscript -\[0-9\]+ is below array bounds" } */
  T (p->a1x[-1].a1);                      /* { dg-warning "array subscript -1 is below array bounds" } */
  T (p->a1x[MAX].a1);
  T (p->a1x[MAX + 2].a1);                 /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */

  T (p->a1x[DIFF_MAX].a1);                /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (p->a1x[SIZE_MAX].a1);                /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */

  T (p->a1x[DIFF_MIN].a1[0]);             /* { dg-warning "array subscript -\[0-9\]+ is below array bounds" } */
  T (p->a1x[-1].a1[0])                    /* { dg-warning "array subscript -1 is below array bounds" } */;
  T (p->a1x[MAX - 1].a1[0]);
  T (p->a1x[MAX].a1[0]);                  /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (p->a1x[MAX + 1].a1[0]);              /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */

  T (p->a1x[DIFF_MAX].a1[0]);             /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
  T (p->a1x[SIZE_MAX].a1[0]);             /* { dg-warning "array subscript \[0-9\]+ is above array bounds" } */
}

void f_cststring (int i)
{
  T (""[DIFF_MIN]);                       /* { dg-warning "array subscript -\[0-9\]+ is below array bounds of .(const )?char *\\\[1]" "string" { xfail { lp64 || llp64 } } } */
  T (""[DIFF_MIN + 1]);                   /* { dg-warning "array subscript -\[0-9\]+ is below array bounds of .(const )?char *\\\[1]" "string" } */
  T (""[-1]);                             /* { dg-warning "array subscript -1 is below array bounds of .(const )?char *\\\[1]" "string" } */
  T (""[0]);
  T (""[1]);                              /* { dg-warning "array subscript 1 is above array bounds of .(const )?char *\\\[1]" "string" } */
  T ("0"[2]);                             /* { dg-warning "array subscript 2 is above array bounds of .(const )?char *\\\[2]" "string" } */
  T ("012"[2]);
  T ("012"[3]);
  T ("012"[4]);                           /* { dg-warning "array subscript 4 is above array bounds of .(const )?char *\\\[4]" "string" } */
  T ("0123"[DIFF_MAX]);                   /* { dg-warning "array subscript \[0-9\]+ is above array bounds of .(const )?char *\\\[5]" "string" } */
  T ("0123"[SIZE_MAX]);                   /* { dg-warning "array subscript \[0-9\]+ is above array bounds of .(const )?char *\\\[5]" "string" } */
}

void fb_strlen (struct B *p)
{
#define strlen __builtin_strlen

  T (strlen (&p->a1x[0].a1[2]));          /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "strlen" } */
  T (strlen (p->a1x[0].a1 + 2));          /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "strlen" { xfail *-*-* } } */
}


void f_vla (unsigned n)
{
  char vla[n];

  T (vla[DIFF_MIN]);                      /* { dg-warning "array subscript -\[0-9\]+ is below array bounds" "vla" } */
  T (vla[-1]);                            /* { dg-warning "array subscript -1 is below array bounds" "vla" } */
  T (vla[0]);
  T (vla[1]);
  T (vla[n - 1]);
  /* It would be nice to diagnose this. */
  T (vla[n]);                             /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "bug 82608" { xfail *-*-*} } */
  T (vla[DIFF_MAX]);                      /* { dg-warning "array subscript \[0-9\]+ is above array bounds" "vla" } */
}
