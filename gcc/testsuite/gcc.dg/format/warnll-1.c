/* Test for printf formats.  C99 "long long" formats should be accepted with
   -Wno-long-long.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat -Wno-long-long" } */

#include "format.h"

void
foo (long long ll, unsigned long long ull, long long *lln,
     long long *llp, unsigned long long *ullp)
{
  /* Test for accepting standard "long long" formats.  */
  printf ("%lld%lli%llo%llu%llx%llX%lln", ll, ll, ull, ull, ull, ull, lln);
  scanf ("%lld%lli%llo%llu%llx%llX%lln", llp, llp,
	 ullp, ullp, ullp, ullp, lln);
  /* Use of "q" and "L" should still be warned about.  */
  printf ("%qd", ll); /* { dg-warning "C" "printf %qd" } */
  printf ("%qi", ll); /* { dg-warning "C" "printf %qi" } */
  printf ("%qo", ull); /* { dg-warning "C" "printf %qo" } */
  printf ("%qu", ull); /* { dg-warning "C" "printf %qu" } */
  printf ("%qx", ull); /* { dg-warning "C" "printf %qx" } */
  printf ("%qX", ull); /* { dg-warning "C" "printf %qX" } */
  printf ("%qn", lln); /* { dg-warning "C" "printf %qn" } */
  printf ("%Ld", ll); /* { dg-warning "C" "printf %Ld" } */
  printf ("%Li", ll); /* { dg-warning "C" "printf %Li" } */
  printf ("%Lo", ull); /* { dg-warning "C" "printf %oL" } */
  printf ("%Lu", ull); /* { dg-warning "C" "printf %Lu" } */
  printf ("%Lx", ull); /* { dg-warning "C" "printf %Lx" } */
  printf ("%LX", ull); /* { dg-warning "C" "printf %LX" } */
  scanf ("%qd", llp); /* { dg-warning "C" "scanf %qd" } */
  scanf ("%qi", llp); /* { dg-warning "C" "scanf %qi" } */
  scanf ("%qo", ullp); /* { dg-warning "C" "scanf %qo" } */
  scanf ("%qu", ullp); /* { dg-warning "C" "scanf %qu" } */
  scanf ("%qx", ullp); /* { dg-warning "C" "scanf %qx" } */
  scanf ("%qX", ullp); /* { dg-warning "C" "scanf %qX" } */
  scanf ("%qn", lln); /* { dg-warning "C" "scanf %qn" } */
  scanf ("%Ld", llp); /* { dg-warning "C" "scanf %Ld" } */
  scanf ("%Li", llp); /* { dg-warning "C" "scanf %Li" } */
  scanf ("%Lo", ullp); /* { dg-warning "C" "scanf %oL" } */
  scanf ("%Lu", ullp); /* { dg-warning "C" "scanf %Lu" } */
  scanf ("%Lx", ullp); /* { dg-warning "C" "scanf %Lx" } */
  scanf ("%LX", ullp); /* { dg-warning "C" "scanf %LX" } */
}
