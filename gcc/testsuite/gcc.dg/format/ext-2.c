/* Test for format extensions beyond the C standard and X/Open standard.
   Test for scanf formats.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

#include "format.h"

void
foo (quad_t *qp, u_quad_t *uqp, quad_t *qn, long long int *llp,
     unsigned long long int *ullp, float *fp, char *s, void **pp, wchar_t *ls,
     int *ip, unsigned int *up)
{
  /* As an extension, GCC allows the BSD length "q" for integer formats.
     This is largely obsoleted in C99 by %j, %ll and SCNd64.
  */
  scanf ("%qd%qi%qo%qu%qx%qX%qn", qp, qp, uqp, uqp, uqp, uqp, qn);
  scanf ("%qf", fp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qF", fp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qe", fp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qE", fp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qg", fp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qG", fp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qa", fp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qA", fp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qs", s); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%q[abc]", s); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qc", s); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qp", pp); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qC", ls); /* { dg-warning "length" "bad use of %q" } */
  scanf ("%qS", ls); /* { dg-warning "length" "bad use of %q" } */
  /* As an extension, GCC allows the length "L" on integer formats
     (but not %n) as a synonym for "ll".
     This should be considered deprecated.
  */
  scanf ("%Ld%Li%Lo%Lu%Lx%LX", llp, llp, ullp, ullp, ullp, ullp);
  /* glibc also supports flags ' and I on scanf formats.  The ' flag applies
     to all formats scanning decimal values; the I flag only to decimal integer
     formats.
  */
  scanf ("%'d%'i%'u%'a%'A%'e%'E%'f%'F%'g%'G", ip, ip, up, fp, fp, fp, fp,
	 fp, fp, fp, fp);
  scanf ("%'o", up); /* { dg-warning "flag" "bad use of ' flag" } */
  scanf ("%'x", up); /* { dg-warning "flag" "bad use of ' flag" } */
  scanf ("%'X", up); /* { dg-warning "flag" "bad use of ' flag" } */
  scanf ("%'n", ip); /* { dg-warning "flag" "bad use of ' flag" } */
  scanf ("%'s", s); /* { dg-warning "flag" "bad use of ' flag" } */
  scanf ("%'[abc]", s); /* { dg-warning "flag" "bad use of ' flag" } */
  scanf ("%'c", s); /* { dg-warning "flag" "bad use of ' flag" } */
  scanf ("%'p", pp); /* { dg-warning "flag" "bad use of ' flag" } */
  scanf ("%'C", ls); /* { dg-warning "flag" "bad use of ' flag" } */
  scanf ("%'S", ls); /* { dg-warning "flag" "bad use of ' flag" } */
  scanf ("%Id%Ii%Iu", ip, ip, up);
  scanf ("%Ia", fp); /* { dg-warning "flag" "bad use of I flag" } */
  scanf ("%IA", fp); /* { dg-warning "flag" "bad use of I flag" } */
  scanf ("%Ie", fp); /* { dg-warning "flag" "bad use of I flag" } */
  scanf ("%IE", fp); /* { dg-warning "flag" "bad use of I flag" } */
  scanf ("%If", fp); /* { dg-warning "flag" "bad use of I flag" } */
  scanf ("%IF", fp); /* { dg-warning "flag" "bad use of I flag" } */
  scanf ("%Ig", fp); /* { dg-warning "flag" "bad use of I flag" } */
  scanf ("%IG", fp); /* { dg-warning "flag" "bad use of I flag" } */
  scanf ("%Io", up); /* { dg-warning "flag" "bad use of I flag" } */
  scanf ("%Ix", up); /* { dg-warning "flag" "bad use of I flag" } */
  scanf ("%IX", up); /* { dg-warning "flag" "bad use of I flag" } */
  scanf ("%In", ip); /* { dg-warning "flag" "bad use of I flag" } */
  scanf ("%Is", s); /* { dg-warning "flag" "bad use of I flag" } */
  scanf ("%I[abc]", s); /* { dg-warning "flag" "bad use of I flag" } */
  scanf ("%Ic", s); /* { dg-warning "flag" "bad use of I flag" } */
  scanf ("%Ip", pp); /* { dg-warning "flag" "bad use of I flag" } */
  scanf ("%IC", ls); /* { dg-warning "flag" "bad use of I flag" } */
  scanf ("%IS", ls); /* { dg-warning "flag" "bad use of I flag" } */
}
