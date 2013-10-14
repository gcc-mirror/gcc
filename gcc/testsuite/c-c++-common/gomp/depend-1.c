/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

extern int a[][10], a2[][10];
int b[10], c[10][2], d[10], e[10], f[10];
int b2[10], c2[10][2], d2[10], e2[10], f2[10];
int k[10], l[10], m[10], n[10], o;
int *p;
void bar (void);
int t[10];
#pragma omp threadprivate (t)

void
foo (int g[3][10], int h[4][8], int i[2][10], int j[][9],
     int g2[3][10], int h2[4][8], int i2[2][10], int j2[][9])
{
  #pragma omp task depend(in: bar[2:5]) /* { dg-error "is not a variable" } */
    ;
  #pragma omp task depend(out: t[2:5])
    ;
  #pragma omp task depend(inout: k[0.5:]) /* { dg-error "low bound \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma omp task depend(in: l[:7.5f]) /* { dg-error "length \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma omp task depend(out: m[p:]) /* { dg-error "low bound \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma omp task depend(inout: n[:p]) /* { dg-error "length \[^\n\r]* of array section does not have integral type" } */
    ;
  #pragma omp task depend(in: o[2:5]) /* { dg-error "does not have pointer or array type" } */
    ;
  #pragma omp task depend(out: a[:][2:4]) /* { dg-error "array type length expression must be specified" } */
    ;
  #pragma omp task depend(inout: b[-1:]) /* { dg-error "negative low bound in array section" } */
    ;
  #pragma omp task depend(inout: c[:-3][1:1]) /* { dg-error "negative length in array section" } */
    ;
  #pragma omp task depend(in: d[11:]) /* { dg-error "low bound \[^\n\r]* above array section size" } */
    ;
  #pragma omp task depend(out: e[:11]) /* { dg-error "length \[^\n\r]* above array section size" } */
    ;
  #pragma omp task depend(out: f[1:10]) /* { dg-error "high bound \[^\n\r]* above array section size" } */
    ;
  #pragma omp task depend(in: g[:][2:4]) /* { dg-error "for pointer type length expression must be specified" } */
    ;
  #pragma omp task depend(in: h[2:2][-1:]) /* { dg-error "negative low bound in array section" } */
    ;
  #pragma omp task depend(inout: h[:1][:-3]) /* { dg-error "negative length in array section" } */
    ;
  #pragma omp task depend(out: i[:1][11:]) /* { dg-error "low bound \[^\n\r]* above array section size" } */
    ;
  #pragma omp task depend(in: j[3:4][:10]) /* { dg-error "length \[^\n\r]* above array section size" } */
    ;
  #pragma omp task depend(out: j[30:10][5:5]) /* { dg-error "high bound \[^\n\r]* above array section size" } */
    ;
  #pragma omp task depend(out: a2[:3][2:4])
    ;
  #pragma omp task depend(inout: b2[0:])
    ;
  #pragma omp task depend(inout: c2[:3][1:1])
    ;
  #pragma omp task depend(in: d2[9:])
    ;
  #pragma omp task depend(out: e2[:10])
    ;
  #pragma omp task depend(out: f2[1:9])
    ;
  #pragma omp task depend(in: g2[:2][2:4])
    ;
  #pragma omp task depend(in: h2[2:2][0:])
    ;
  #pragma omp task depend(inout: h2[:1][:3])
    ;
  #pragma omp task depend(out: i2[:1][9:])
    ;
  #pragma omp task depend(in: j2[3:4][:9])
    ;
  #pragma omp task depend(out: j2[30:10][5:4])
    ;
}
