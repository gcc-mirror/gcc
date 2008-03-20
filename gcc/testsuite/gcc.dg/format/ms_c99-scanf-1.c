/* Test for scanf formats.  Formats using C99 features, including cases
   where C99 specifies some aspect of the format to be ignored or where
   the behavior is undefined.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile { target { *-*-mingw* } } } */
/* { dg-options "-std=iso9899:1999 -pedantic -Wformat" } */

#define USE_SYSTEM_FORMATS
#include "format.h"

void
foo (int *ip, unsigned int *uip, short int *hp, unsigned short int *uhp,
     signed char *hhp, unsigned char *uhhp, long int *lp,
     unsigned long int *ulp, float *fp, double *dp, long double *ldp, char *s,
     void **pp, int *n, long long *llp, unsigned long long *ullp, wchar_t *ls,
     short int *hn, signed char *hhn, long int *ln, long long int *lln,
     intmax_t *jp, uintmax_t *ujp, intmax_t *jn, size_t *zp,
     signed_size_t *szp, signed_size_t *zn, ptrdiff_t *tp,
     unsigned_ptrdiff_t *utp, ptrdiff_t *tn)
{
  /* See ISO/IEC 9899:1999 (E) subclause 7.19.6.2 (pages 281-288).
     We do not repeat here most of the checks for correct C90 formats
     or completely broken formats.
  */
  /* Valid, invalid and silly assignment-suppression
     and width constructions.
  */
  scanf ("%*d%*i%*o%*u%*x%*X%*e%*E%*f%*g%*G%*s%*[abc]%*c%*p");
  scanf ("%*2d%*8s%*3c");
  scanf ("%*n", n); /* { dg-warning "suppress" "suppression of %n" } */
  scanf ("%*hd"); /* { dg-warning "together" "suppression with length" } */
  scanf ("%2d%3i%4o%5u%6x%7X%10e%11E%12f%14g%15G%16s%3[abc]%4c%5p",
	 ip, ip, uip, uip, uip, uip, fp, fp, fp, fp, fp,
	 s, s, s, pp);
  scanf ("%0d", ip); /* { dg-warning "width" "warning for zero width" } */
  scanf ("%3n", n); /* { dg-warning "width" "width with %n" } */
  /* Valid and invalid %h, %hh, %l, %j, %z, %t, %L constructions.  */
  scanf ("%hd%hi%ho%hu%hx%hX%hn", hp, hp, uhp, uhp, uhp, uhp, hn);
  scanf ("%he", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hE", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hf", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hg", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hG", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hs", hp);
  scanf ("%h[ac]", s); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hc", (short *)s);
  scanf ("%hp", pp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hhd", hhp); /* { dg-warning "unknown|format" "%hh is unsupported" } */
  scanf ("%ld%li%lo%lu%lx%lX%ln", lp, lp, ulp, ulp, ulp, ulp, ln);
  scanf ("%le%lE%lf%lg%lG", dp, dp, dp, dp, dp);
  scanf ("%lp", pp); /* { dg-warning "length" "bad use of %l" } */
  scanf ("%ls", ls);
  scanf ("%l[ac]", ls);
  scanf ("%lc", ls);
  scanf ("%jd", jp); /* { dg-warning "unknown|format" "%j not supported" } */
  scanf ("%zd", zp); /* { dg-warning "unknown|format" "%z not supported" } */
  scanf ("%td", tp); /* { dg-warning "unknown|format" "%t not supported" } */
  scanf ("%Lf", llp); /* { dg-warning "unknown|format" "bad use of %L is not supported" } */
  /* Valid uses of each bare conversion.  */
  scanf ("%d%i%o%u%x%X%e%E%f%g%G%s%[abc]%c%p%n%%", ip, ip, uip, uip, uip,
         uip, fp, fp, fp, fp, fp, s, s, s, pp, n);
}
