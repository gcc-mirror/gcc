/* Test for scanf formats.  Formats using C99 features, including cases
   where C99 specifies some aspect of the format to be ignored or where
   the behaviour is undefined.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic -Wformat" } */

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
  scanf ("%*d%*i%*o%*u%*x%*X%*a%*A%*e%*E%*f%*F%*g%*G%*s%*[abc]%*c%*p");
  scanf ("%*2d%*8s%*3c");
  scanf ("%*n", n); /* { dg-warning "suppress" "suppression of %n" } */
  scanf ("%*hd"); /* { dg-warning "together" "suppression with length" } */
  scanf ("%2d%3i%4o%5u%6x%7X%8a%9A%10e%11E%12f%13F%14g%15G%16s%3[abc]%4c%5p",
	 ip, ip, uip, uip, uip, uip, fp, fp, fp, fp, fp, fp, fp, fp,
	 s, s, s, pp);
  scanf ("%0d", ip); /* { dg-warning "width" "warning for zero width" } */
  scanf ("%3n", n); /* { dg-warning "width" "width with %n" } */
  /* Valid and invalid %h, %hh, %l, %ll, %j, %z, %t, %L constructions.  */
  scanf ("%hd%hi%ho%hu%hx%hX%hn", hp, hp, uhp, uhp, uhp, uhp, hn);
  scanf ("%ha", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hA", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%he", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hE", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hf", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hF", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hg", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hG", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hs", s); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%h[ac]", s); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hc", s); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hp", pp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hhd%hhi%hho%hhu%hhx%hhX%hhn", hhp, hhp, uhhp, uhhp, uhhp, uhhp,
	 hhn);
  scanf ("%hha", fp); /* { dg-warning "length" "bad use of %hh" } */
  scanf ("%hhA", fp); /* { dg-warning "length" "bad use of %hh" } */
  scanf ("%hhe", fp); /* { dg-warning "length" "bad use of %hh" } */
  scanf ("%hhE", fp); /* { dg-warning "length" "bad use of %hh" } */
  scanf ("%hhf", fp); /* { dg-warning "length" "bad use of %hh" } */
  scanf ("%hhF", fp); /* { dg-warning "length" "bad use of %hh" } */
  scanf ("%hhg", fp); /* { dg-warning "length" "bad use of %hh" } */
  scanf ("%hhG", fp); /* { dg-warning "length" "bad use of %hh" } */
  scanf ("%hhs", s); /* { dg-warning "length" "bad use of %hh" } */
  scanf ("%hh[ac]", s); /* { dg-warning "length" "bad use of %hh" } */
  scanf ("%hhc", s); /* { dg-warning "length" "bad use of %hh" } */
  scanf ("%hhp", pp); /* { dg-warning "length" "bad use of %hh" } */
  scanf ("%ld%li%lo%lu%lx%lX%ln", lp, lp, ulp, ulp, ulp, ulp, ln);
  scanf ("%la%lA%le%lE%lf%lF%lg%lG", dp, dp, dp, dp, dp, dp, dp, dp);
  scanf ("%lp", pp); /* { dg-warning "length" "bad use of %l" } */
  scanf ("%ls", ls);
  scanf ("%l[ac]", ls);
  scanf ("%lc", ls);
  scanf ("%lld%lli%llo%llu%llx%llX%lln", llp, llp, ullp, ullp, ullp, ullp,
	 lln);
  scanf ("%lla", fp); /* { dg-warning "length" "bad use of %ll" } */
  scanf ("%llA", fp); /* { dg-warning "length" "bad use of %ll" } */
  scanf ("%lle", fp); /* { dg-warning "length" "bad use of %ll" } */
  scanf ("%llE", fp); /* { dg-warning "length" "bad use of %ll" } */
  scanf ("%llf", fp); /* { dg-warning "length" "bad use of %ll" } */
  scanf ("%llF", fp); /* { dg-warning "length" "bad use of %ll" } */
  scanf ("%llg", fp); /* { dg-warning "length" "bad use of %ll" } */
  scanf ("%llG", fp); /* { dg-warning "length" "bad use of %ll" } */
  scanf ("%lls", s); /* { dg-warning "length" "bad use of %ll" } */
  scanf ("%ll[ac]", s); /* { dg-warning "length" "bad use of %ll" } */
  scanf ("%llc", s); /* { dg-warning "length" "bad use of %ll" } */
  scanf ("%llp", pp); /* { dg-warning "length" "bad use of %ll" } */
  scanf ("%jd%ji%jo%ju%jx%jX%jn", jp, jp, ujp, ujp, ujp, ujp, jn); /* { dg-bogus "length" "bogus %j warning" { target *-*-* } } */
  scanf ("%ja", fp); /* { dg-warning "length" "bad use of %j" } */
  scanf ("%jA", fp); /* { dg-warning "length" "bad use of %j" } */
  scanf ("%je", fp); /* { dg-warning "length" "bad use of %j" } */
  scanf ("%jE", fp); /* { dg-warning "length" "bad use of %j" } */
  scanf ("%jf", fp); /* { dg-warning "length" "bad use of %j" } */
  scanf ("%jF", fp); /* { dg-warning "length" "bad use of %j" } */
  scanf ("%jg", fp); /* { dg-warning "length" "bad use of %j" } */
  scanf ("%jG", fp); /* { dg-warning "length" "bad use of %j" } */
  scanf ("%js", s); /* { dg-warning "length" "bad use of %j" } */
  scanf ("%j[ac]", s); /* { dg-warning "length" "bad use of %j" } */
  scanf ("%jc", s); /* { dg-warning "length" "bad use of %j" } */
  scanf ("%jp", pp); /* { dg-warning "length" "bad use of %j" } */
  scanf ("%zd%zi%zo%zu%zx%zX%zn", szp, szp, zp, zp, zp, zp, zn);
  scanf ("%za", fp); /* { dg-warning "length" "bad use of %z" } */
  scanf ("%zA", fp); /* { dg-warning "length" "bad use of %z" } */
  scanf ("%ze", fp); /* { dg-warning "length" "bad use of %z" } */
  scanf ("%zE", fp); /* { dg-warning "length" "bad use of %z" } */
  scanf ("%zf", fp); /* { dg-warning "length" "bad use of %z" } */
  scanf ("%zF", fp); /* { dg-warning "length" "bad use of %z" } */
  scanf ("%zg", fp); /* { dg-warning "length" "bad use of %z" } */
  scanf ("%zG", fp); /* { dg-warning "length" "bad use of %z" } */
  scanf ("%zs", s); /* { dg-warning "length" "bad use of %z" } */
  scanf ("%z[ac]", s); /* { dg-warning "length" "bad use of %z" } */
  scanf ("%zc", s); /* { dg-warning "length" "bad use of %z" } */
  scanf ("%zp", pp); /* { dg-warning "length" "bad use of %z" } */
  scanf ("%td%ti%to%tu%tx%tX%tn", tp, tp, utp, utp, utp, utp, tn);
  scanf ("%ta", fp); /* { dg-warning "length" "bad use of %t" } */
  scanf ("%tA", fp); /* { dg-warning "length" "bad use of %t" } */
  scanf ("%te", fp); /* { dg-warning "length" "bad use of %t" } */
  scanf ("%tE", fp); /* { dg-warning "length" "bad use of %t" } */
  scanf ("%tf", fp); /* { dg-warning "length" "bad use of %t" } */
  scanf ("%tF", fp); /* { dg-warning "length" "bad use of %t" } */
  scanf ("%tg", fp); /* { dg-warning "length" "bad use of %t" } */
  scanf ("%tG", fp); /* { dg-warning "length" "bad use of %t" } */
  scanf ("%ts", s); /* { dg-warning "length" "bad use of %t" } */
  scanf ("%t[ac]", s); /* { dg-warning "length" "bad use of %t" } */
  scanf ("%tc", s); /* { dg-warning "length" "bad use of %t" } */
  scanf ("%tp", pp); /* { dg-warning "length" "bad use of %t" } */
  scanf ("%La%LA%Le%LE%Lf%LF%Lg%LG", ldp, ldp, ldp, ldp, ldp, ldp, ldp, ldp);
  scanf ("%Ld", llp); /* { dg-warning "does not support" "bad use of %L" } */
  scanf ("%Li", llp); /* { dg-warning "does not support" "bad use of %L" } */
  scanf ("%Lo", ullp); /* { dg-warning "does not support" "bad use of %L" } */
  scanf ("%Lu", ullp); /* { dg-warning "does not support" "bad use of %L" } */
  scanf ("%Lx", ullp); /* { dg-warning "does not support" "bad use of %L" } */
  scanf ("%LX", ullp); /* { dg-warning "does not support" "bad use of %L" } */
  scanf ("%Ls", s); /* { dg-warning "length" "bad use of %L" } */
  scanf ("%L[ac]", s); /* { dg-warning "length" "bad use of %L" } */
  scanf ("%Lc", s); /* { dg-warning "length" "bad use of %L" } */
  scanf ("%Lp", pp); /* { dg-warning "length" "bad use of %L" } */
  scanf ("%Ln", n); /* { dg-warning "length" "bad use of %L" } */
  /* Valid uses of each bare conversion.  */
  scanf ("%d%i%o%u%x%X%a%A%e%E%f%F%g%G%s%[abc]%c%p%n%%", ip, ip, uip, uip, uip,
         uip, fp, fp, fp, fp, fp, fp, fp, fp, s, s, s, pp, n);
  /* Assert that %as is not treated as an extension in C99 mode.  */
  scanf ("%as", fp);
  scanf ("%a[", fp);
  /* Tests for bad argument types: pointer target sign with %hh.  */
  scanf ("%hhd", uhhp); /* { dg-warning "format" "%hhd sign" } */
  scanf ("%hhu", hhp); /* { dg-warning "format" "%hhu sign" } */
}
