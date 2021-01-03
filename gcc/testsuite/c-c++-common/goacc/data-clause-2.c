/* Test data clause diagnostics.  */

/* See also corresponding OpenACC C++ variant: '../../g++.dg/goacc/data-clause-2.C'.  */

/* See also corresponding OpenACC 'cache' directive variant: 'cache-3-2.c'.  */

/* See also corresponding OpenMP variant: '../gomp/map-2.c'.  */

void
foo (int *p, int (*q)[10], int r[10], int s[10][10])
{
  int a[10], b[10][10];
  #pragma acc parallel copy (p[-1:2])
  ;
  #pragma acc parallel copy (q[-1:2][0:10])
  ;
  #pragma acc parallel copy (q[-1:2][-2:10]) /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma acc parallel copy (r[-1:2])
  ;
  #pragma acc parallel copy (s[-1:2][:])
  ;
  #pragma acc parallel copy (s[-1:2][-2:10]) /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma acc parallel copy (a[-1:2])	 /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma acc parallel copy (b[-1:2][0:])	 /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma acc parallel copy (b[1:2][-2:10]) /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma acc parallel copy (p[2:-3])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma acc parallel copy (q[2:-3][:])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma acc parallel copy (q[2:3][0:-1])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma acc parallel copy (r[2:-5])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma acc parallel copy (s[2:-5][:])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma acc parallel copy (s[2:5][0:-4])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma acc parallel copy (a[2:-5])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma acc parallel copy (b[2:-5][0:10]) /* { dg-error "negative length in array section in" } */
  ;
  #pragma acc parallel copy (b[2:5][0:-4]) /* { dg-error "negative length in array section in" } */
  ;
}
