/* Test 'cache' directive diagnostics.  */

/* See also corresponding C++ variant: '../../g++.dg/goacc/cache-3-2.C'.  */

/* See also corresponding C/C++ data clause variant: 'data-clause-2.c'.  */

/* The current implementation doesn't restrict where a 'cache' directive may
   appear, so we don't make any special arrangements.  */

void
foo (int *p, int (*q)[10], int r[10], int s[10][10])
{
  int a[10], b[10][10];
  #pragma acc cache (p[-1:2])
  ;
  #pragma acc cache (q[-1:2][0:10])
  ;
  #pragma acc cache (q[-1:2][-2:10]) /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma acc cache (r[-1:2])
  ;
  #pragma acc cache (s[-1:2][:])
  ;
  #pragma acc cache (s[-1:2][-2:10]) /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma acc cache (a[-1:2])	 /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma acc cache (b[-1:2][0:])	 /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma acc cache (b[1:2][-2:10]) /* { dg-error "negative low bound in array section in" } */
  ;
  #pragma acc cache (p[2:-3])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma acc cache (q[2:-3][:])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma acc cache (q[2:3][0:-1])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma acc cache (r[2:-5])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma acc cache (s[2:-5][:])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma acc cache (s[2:5][0:-4])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma acc cache (a[2:-5])	 /* { dg-error "negative length in array section in" } */
  ;
  #pragma acc cache (b[2:-5][0:10]) /* { dg-error "negative length in array section in" } */
  ;
  #pragma acc cache (b[2:5][0:-4]) /* { dg-error "negative length in array section in" } */
  ;
}
