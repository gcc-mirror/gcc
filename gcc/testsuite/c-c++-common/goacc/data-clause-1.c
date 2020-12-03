/* Test data clause diagnostics.  */

/* See also corresponding OpenACC C++ variant: '../../g++.dg/goacc/data-clause-1.C'.  */

/* See also corresponding OpenACC 'cache' directive variant: 'cache-3-1.c'.  */

/* See also corresponding OpenMP variant: '../gomp/map-1.c'.  */

/* { dg-additional-options "-fopenmp" } for '#pragma omp threadprivate'.  */

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

void
foo (int g[3][10], int h[4][8], int i[2][10], int j[][9],
     int g2[3][10], int h2[4][8], int i2[2][10], int j2[][9])
{
  #pragma acc parallel copyin(bar[2:5]) /* { dg-error "is not a variable" } */
    ;
  #pragma acc parallel copyout(t[2:5]) /* { dg-error "is threadprivate variable" } */
    ;
  #pragma acc parallel copy(k[0.5:]) /* { dg-error "low bound \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma acc parallel copyout(l[:7.5f]) /* { dg-error "length \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma acc parallel copyin(m[p:]) /* { dg-error "low bound \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma acc parallel copy(n[:p]) /* { dg-error "length \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma acc parallel copyin(o[2:5]) /* { dg-error "does not have pointer or array type" } */
    ;
  #pragma acc parallel create(s1) /* { dg-error "'s1' does not have a mappable type in 'map' clause" } */
    ;
  #pragma acc parallel create(s2) /* { dg-error "'s2' does not have a mappable type in 'map' clause" } */
    ;
  #pragma acc parallel copyin(a[:][:]) /* { dg-error "array type length expression must be specified" } */
    bar (&a[0][0]); /* { dg-error "referenced in target region does not have a mappable type" } */
  #pragma acc parallel copy(b[-1:]) /* { dg-error "negative low bound in array section" } */
    bar (b);
  #pragma acc parallel copy(c[:-3][:]) /* { dg-error "negative length in array section" } */
    bar (&c[0][0]);
  #pragma acc parallel copyout(d[11:]) /* { dg-error "low bound \[^\n\r]* above array section size" } */
    bar (d);
  #pragma acc parallel copyin(e[:11]) /* { dg-error "length \[^\n\r]* above array section size" } */
    bar (e);
  #pragma acc parallel copyin(f[1:10]) /* { dg-error "high bound \[^\n\r]* above array section size" } */
    bar (f);
  #pragma acc parallel copyout(g[:][0:10]) /* { dg-error "for array function parameter length expression must be specified" } */
    bar (&g[0][0]);
  #pragma acc parallel copyout(h[2:1][-1:]) /* { dg-error "negative low bound in array section" } */
    bar (&h[0][0]);
  #pragma acc parallel copy(h[:1][:-3]) /* { dg-error "negative length in array section" } */
    bar (&h[0][0]);
  #pragma acc parallel copy(i[:1][11:]) /* { dg-error "low bound \[^\n\r]* above array section size" } */
    bar (&i[0][0]);
  #pragma acc parallel copyout(j[3:1][:10]) /* { dg-error "length \[^\n\r]* above array section size" } */
    bar (&j[0][0]);
  #pragma acc parallel copyin(j[30:1][5:5]) /* { dg-error "high bound \[^\n\r]* above array section size" } */
    bar (&j[0][0]);
  #pragma acc parallel copyin(a2[:1][2:4])
    bar (&a2[0][0]);
  #pragma acc parallel copy(a2[3:5][:])
    bar (&a2[0][0]);
  #pragma acc parallel copyin(a2[3:5][:10])
    bar (&a2[0][0]);
  #pragma acc parallel copy(b2[0:])
    bar (b2);
  #pragma acc parallel copy(c2[:3][:])
    bar (&c2[0][0]);
  #pragma acc parallel copyout(d2[9:])
    bar (d2);
  #pragma acc parallel copyin(e2[:10])
    bar (e2);
  #pragma acc parallel copyin(f2[1:9])
    bar (f2);
  #pragma acc parallel copy(g2[:1][2:4])
    bar (&g2[0][0]);
  #pragma acc parallel copyout(h2[2:2][0:])
    bar (&h2[0][0]);
  #pragma acc parallel copy(h2[:1][:3])
    bar (&h2[0][0]);
  #pragma acc parallel copyin(i2[:1][9:])
    bar (&i2[0][0]);
  #pragma acc parallel copyout(j2[3:4][:9])
    bar (&j2[0][0]);
  #pragma acc parallel copyin(j2[30:1][5:4])
    bar (&j2[0][0]);
  #pragma acc parallel copy(q[1:2])
    ;
  #pragma acc parallel copy(q[3:5][:10]) /* { dg-error "array section is not contiguous" } */
    ;
  #pragma acc parallel copy(r[3:][2:1][1:2])
    ;
  #pragma acc parallel copy(r[3:][2:1][1:2][:][0:4])
    ;
  #pragma acc parallel copy(r[3:][2:1][1:2][1:][0:4]) /* { dg-error "array section is not contiguous" } */
    ;
  #pragma acc parallel copy(r[3:][2:1][1:2][:3][0:4]) /* { dg-error "array section is not contiguous" } */
    ;
  #pragma acc parallel copy(r[3:][2:1][1:2][:][1:]) /* { dg-error "array section is not contiguous" } */
    ;
  #pragma acc parallel copy(r[3:][2:1][1:2][:][:3]) /* { dg-error "array section is not contiguous" } */
    ;
}
