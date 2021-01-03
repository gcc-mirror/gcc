/* Test 'map' clause diagnostics.  */

/* See also corresponding OpenMP C/C++ variant: '../../c-c++-common/gomp/map-2.c'.  */

/* See also corresponding OpenACC variant: '../goacc/data-clause-2.C'.  */

template <int N>
void
foo (int *p, int (*q)[10], int r[10], int s[10][10])
{
  int a[10], b[10][10];
  #pragma omp target map (tofrom: p[-1:2])
  ;
  #pragma omp target map (tofrom: q[-1:2][0:10])
  ;
  #pragma omp target map (tofrom: q[-1:2][-2:10]) /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma omp target map (tofrom: r[-1:2])
  ;
  #pragma omp target map (tofrom: s[-1:2][:])
  ;
  #pragma omp target map (tofrom: s[-1:2][-2:10]) /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma omp target map (tofrom: a[-1:2])	 /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma omp target map (tofrom: b[-1:2][0:])	 /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma omp target map (tofrom: b[1:2][-2:10]) /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma omp target map (tofrom: p[2:-3])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp target map (tofrom: q[2:-3][:])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp target map (tofrom: q[2:3][0:-1])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp target map (tofrom: r[2:-5])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp target map (tofrom: s[2:-5][:])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp target map (tofrom: s[2:5][0:-4])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp target map (tofrom: a[2:-5])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp target map (tofrom: b[2:-5][0:10]) /* { dg-error "negative length in array section in" } */
  ;
  #pragma omp target map (tofrom: b[2:5][0:-4]) /* { dg-error "negative length in array section in" } */
  ;
}

static void
instantiate ()
{
  &foo<0>;
}
