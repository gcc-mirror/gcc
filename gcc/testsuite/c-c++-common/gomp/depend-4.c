/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void
foo (int *p, int (*q)[10], int r[10], int s[10][10])
{
  int a[10], b[10][10];
  #pragma omp task depend (inout: p[-1:2])
  ;
  #pragma omp task depend (inout: q[-1:2][2:4])
  ;
  #pragma omp task depend (inout: q[-1:2][-2:4]) /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma omp task depend (inout: r[-1:2])
  ;
  #pragma omp task depend (inout: s[-1:2][2:4])
  ;
  #pragma omp task depend (inout: s[-1:2][-2:4]) /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma omp task depend (inout: a[-1:2])	 /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma omp task depend (inout: b[-1:2][2:4])	 /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma omp task depend (inout: b[1:2][-2:4])	 /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma omp task depend (inout: p[2:-3])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp task depend (inout: q[2:-3][:])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp task depend (inout: q[2:3][0:-1])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp task depend (inout: r[2:-5])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp task depend (inout: s[2:-5][:])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp task depend (inout: s[2:5][0:-4])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp task depend (inout: a[2:-5])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp task depend (inout: b[2:-5][0:10]) /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp task depend (inout: b[2:5][0:-4]) /* { dg-error "negative length in array section in" } */
  ;
}
