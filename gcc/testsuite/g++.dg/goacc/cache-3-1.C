/* Test 'cache' directive diagnostics.  */

/* See also corresponding C/C++ variant: '../../c-c++-common/goacc/cache-3-1.c'.  */

/* See also corresponding C++ data clause variant: 'data-clause-1.C'.  */

/* { dg-additional-options "-fopenmp" } for '#pragma omp threadprivate'.  */

/* The current implementation doesn't restrict where a 'cache' directive may
   appear, so we don't make any special arrangements.  */

extern int a[][10], a2[][10];
int b[10], c[10][2], d[10], e[10], f[10];
int b2[10], c2[10][2], d2[10], e2[10], f2[10];
int k[10], l[10], m[10], n[10], o;
int *p;
int **q;
int r[4][4][4][4][4];
extern struct s s1;
extern struct s s2[1]; /* { dg-error "array type has incomplete element type" "" { target c } } */
int t[10];
#pragma omp threadprivate (t)
#pragma acc routine
void bar (int *);

template <int N>
void
foo (int g[3][10], int h[4][8], int i[2][10], int j[][9],
     int g2[3][10], int h2[4][8], int i2[2][10], int j2[][9])
{
  #pragma acc cache(bar[2:5]) /* { dg-error "is not a variable" } */
    ;
  #pragma acc cache(t[2:5]) /* { dg-error "is threadprivate variable" } */
    ;
  #pragma acc cache(k[0.5:]) /* { dg-error "low bound \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma acc cache(l[:7.5f]) /* { dg-error "length \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma acc cache(m[p:]) /* { dg-error "low bound \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma acc cache(n[:p]) /* { dg-error "length \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma acc cache(o[2:5]) /* { dg-error "does not have pointer or array type" } */
    ;
  #pragma acc cache(s1) /* { dg-error "expected '\\\['" } */
    ;
  #pragma acc cache(s2) /* { dg-error "expected '\\\['" } */
    ;
  #pragma acc cache(a[:][:]) /* { dg-error "array type length expression must be specified" } */
    bar (&a[0][0]);
  #pragma acc cache(b[-1:]) /* { dg-error "negative low bound in array section" } */
    bar (b);
  #pragma acc cache(c[:-3][:]) /* { dg-error "negative length in array section" } */
    bar (&c[0][0]);
  #pragma acc cache(d[11:]) /* { dg-error "low bound \[^\n\r]* above array section size" } */
    bar (d);
  #pragma acc cache(e[:11]) /* { dg-error "length \[^\n\r]* above array section size" } */
    bar (e);
  #pragma acc cache(f[1:10]) /* { dg-error "high bound \[^\n\r]* above array section size" } */
    bar (f);
  #pragma acc cache(g[:][0:10]) /* { dg-error "for array function parameter length expression must be specified" } */
    bar (&g[0][0]);
  #pragma acc cache(h[2:1][-1:]) /* { dg-error "negative low bound in array section" } */
    bar (&h[0][0]);
  #pragma acc cache(h[:1][:-3]) /* { dg-error "negative length in array section" } */
    bar (&h[0][0]);
  #pragma acc cache(i[:1][11:]) /* { dg-error "low bound \[^\n\r]* above array section size" } */
    bar (&i[0][0]);
  #pragma acc cache(j[3:1][:10]) /* { dg-error "length \[^\n\r]* above array section size" } */
    bar (&j[0][0]);
  #pragma acc cache(j[30:1][5:5]) /* { dg-error "high bound \[^\n\r]* above array section size" } */
    bar (&j[0][0]);
  #pragma acc cache(a2[:1][2:4])
    bar (&a2[0][0]);
  #pragma acc cache(a2[3:5][:])
    bar (&a2[0][0]);
  #pragma acc cache(a2[3:5][:10])
    bar (&a2[0][0]);
  #pragma acc cache(b2[0:])
    bar (b2);
  #pragma acc cache(c2[:3][:])
    bar (&c2[0][0]);
  #pragma acc cache(d2[9:])
    bar (d2);
  #pragma acc cache(e2[:10])
    bar (e2);
  #pragma acc cache(f2[1:9])
    bar (f2);
  #pragma acc cache(g2[:1][2:4])
    bar (&g2[0][0]);
  #pragma acc cache(h2[2:2][0:])
    bar (&h2[0][0]);
  #pragma acc cache(h2[:1][:3])
    bar (&h2[0][0]);
  #pragma acc cache(i2[:1][9:])
    bar (&i2[0][0]);
  #pragma acc cache(j2[3:4][:9])
    bar (&j2[0][0]);
  #pragma acc cache(j2[30:1][5:4])
    bar (&j2[0][0]);
  #pragma acc cache(q[1:2])
    ;
  #pragma acc cache(q[3:5][:10]) /* { dg-error "array section is not contiguous" } */
    ;
  #pragma acc cache(r[3:][2:1][1:2])
    ;
  #pragma acc cache(r[3:][2:1][1:2][:][0:4])
    ;
  #pragma acc cache(r[3:][2:1][1:2][1:][0:4]) /* { dg-error "array section is not contiguous" } */
    ;
  #pragma acc cache(r[3:][2:1][1:2][:3][0:4]) /* { dg-error "array section is not contiguous" } */
    ;
  #pragma acc cache(r[3:][2:1][1:2][:][1:]) /* { dg-error "array section is not contiguous" } */
    ;
  #pragma acc cache(r[3:][2:1][1:2][:][:3]) /* { dg-error "array section is not contiguous" } */
    ;
}

static void
instantiate ()
{
  &foo<0>;
}
