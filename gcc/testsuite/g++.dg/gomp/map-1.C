/* Test 'map' clause diagnostics.  */

/* See also corresponding OpenMP C/C++ variant: '../../c-c++-common/gomp/map-1.c'.  */

/* See also corresponding OpenACC variant: '../goacc/data-clause-1.C'.  */

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
#pragma omp declare target
void bar (int *);
#pragma omp end declare target

template <int N>
void
foo (int g[3][10], int h[4][8], int i[2][10], int j[][9],
     int g2[3][10], int h2[4][8], int i2[2][10], int j2[][9])
{
  #pragma omp target map(to: bar[2:5]) /* { dg-error "is not a variable" } */
    ;
  #pragma omp target map(from: t[2:5]) /* { dg-error "is threadprivate variable" } */
    ;
  #pragma omp target map(tofrom: k[0.5:]) /* { dg-error "low bound \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma omp target map(from: l[:7.5f]) /* { dg-error "length \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma omp target map(to: m[p:]) /* { dg-error "low bound \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma omp target map(tofrom: n[:p]) /* { dg-error "length \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma omp target map(to: o[2:5]) /* { dg-error "does not have pointer or array type" } */
    ;
  #pragma omp target map(alloc: s1) /* { dg-error "'s1' does not have a mappable type in 'map' clause" } */
    ;
  #pragma omp target map(alloc: s2) /* { dg-error "'s2' does not have a mappable type in 'map' clause" } */
    ;
  #pragma omp target map(to: a[:][:]) /* { dg-error "array type length expression must be specified" } */
    bar (&a[0][0]); /* { dg-error "referenced in target region does not have a mappable type" "PR97996" { xfail *-*-* } } */
  #pragma omp target map(tofrom: b[-1:]) /* { dg-error "negative low bound in array section" } */
    bar (b);
  #pragma omp target map(tofrom: c[:-3][:]) /* { dg-error "negative length in array section" } */
    bar (&c[0][0]);
  #pragma omp target map(from: d[11:]) /* { dg-error "low bound \[^\n\r]* above array section size" } */
    bar (d);
  #pragma omp target map(to: e[:11]) /* { dg-error "length \[^\n\r]* above array section size" } */
    bar (e);
  #pragma omp target map(to: f[1:10]) /* { dg-error "high bound \[^\n\r]* above array section size" } */
    bar (f);
  #pragma omp target map(from: g[:][0:10]) /* { dg-error "for array function parameter length expression must be specified" } */
    bar (&g[0][0]);
  #pragma omp target map(from: h[2:1][-1:]) /* { dg-error "negative low bound in array section" } */
    bar (&h[0][0]);
  #pragma omp target map(tofrom: h[:1][:-3]) /* { dg-error "negative length in array section" } */
    bar (&h[0][0]);
  #pragma omp target map(i[:1][11:]) /* { dg-error "low bound \[^\n\r]* above array section size" } */
    bar (&i[0][0]);
  #pragma omp target map(from: j[3:1][:10]) /* { dg-error "length \[^\n\r]* above array section size" } */
    bar (&j[0][0]);
  #pragma omp target map(to: j[30:1][5:5]) /* { dg-error "high bound \[^\n\r]* above array section size" } */
    bar (&j[0][0]);
  #pragma omp target map(to: a2[:1][2:4])
    bar (&a2[0][0]);
  #pragma omp target map(a2[3:5][:])
    bar (&a2[0][0]);
  #pragma omp target map(to: a2[3:5][:10])
    bar (&a2[0][0]);
  #pragma omp target map(tofrom: b2[0:])
    bar (b2);
  #pragma omp target map(tofrom: c2[:3][:])
    bar (&c2[0][0]);
  #pragma omp target map(from: d2[9:])
    bar (d2);
  #pragma omp target map(to: e2[:10])
    bar (e2);
  #pragma omp target map(to: f2[1:9])
    bar (f2);
  #pragma omp target map(g2[:1][2:4])
    bar (&g2[0][0]);
  #pragma omp target map(from: h2[2:2][0:])
    bar (&h2[0][0]);
  #pragma omp target map(tofrom: h2[:1][:3])
    bar (&h2[0][0]);
  #pragma omp target map(to: i2[:1][9:])
    bar (&i2[0][0]);
  #pragma omp target map(from: j2[3:4][:9])
    bar (&j2[0][0]);
  #pragma omp target map(to: j2[30:1][5:4])
    bar (&j2[0][0]);
  #pragma omp target map(q[1:2])
    ;
  #pragma omp target map(tofrom: q[3:5][:10]) /* { dg-error "array section is not contiguous" } */
    ;
  #pragma omp target map(r[3:][2:1][1:2])
    ;
  #pragma omp target map(r[3:][2:1][1:2][:][0:4])
    ;
  #pragma omp target map(r[3:][2:1][1:2][1:][0:4]) /* { dg-error "array section is not contiguous" } */
    ;
  #pragma omp target map(r[3:][2:1][1:2][:3][0:4]) /* { dg-error "array section is not contiguous" } */
    ;
  #pragma omp target map(r[3:][2:1][1:2][:][1:]) /* { dg-error "array section is not contiguous" } */
    ;
  #pragma omp target map(r[3:][2:1][1:2][:][:3]) /* { dg-error "array section is not contiguous" } */
    ;
}

static void
instantiate ()
{
  &foo<0>;
}
