/* Test for scanf formats.  Formats using C90 features, including cases
   where C90 specifies some aspect of the format to be ignored or where
   the behaviour is undefined.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat" } */

#include "format.h"

void
foo (int *ip, unsigned int *uip, short int *hp, unsigned short int *uhp,
     long int *lp, unsigned long int *ulp, float *fp, double *dp,
     long double *ldp, char *s, signed char *ss, unsigned char *us,
     void **pp, int *n, llong *llp, ullong *ullp, wchar_t *ls,
     const int *cip, const int *cn, const char *cs, const void **ppc,
     void *const *pcp, short int *hn, long int *ln, void *p, char **sp,
     volatile void *ppv)
{
  /* See ISO/IEC 9899:1990 (E) subclause 7.9.6.2 (pages 134-138).  */
  /* Basic sanity checks for the different components of a format.  */
  scanf ("%d", ip);
  scanf ("%*d");
  scanf ("%3d", ip);
  scanf ("%hd", hp);
  scanf ("%3ld", lp);
  scanf ("%*3d");
  scanf ("%d %ld", ip, lp);
  /* Valid and invalid %% constructions.  */
  scanf ("%%");
  scanf ("%*%"); /* { dg-warning "format" "bogus %%" } */
  scanf ("%*%\n"); /* { dg-warning "format" "bogus %%" } */
  scanf ("%4%"); /* { dg-warning "format" "bogus %%" } */
  scanf ("%4%\n"); /* { dg-warning "format" "bogus %%" } */
  scanf ("%h%"); /* { dg-warning "format" "bogus %%" } */
  scanf ("%h%\n"); /* { dg-warning "format" "bogus %%" } */
  /* Valid, invalid and silly assignment-suppression constructions.  */
  scanf ("%*d%*i%*o%*u%*x%*X%*e%*E%*f%*g%*G%*s%*[abc]%*c%*p");
  scanf ("%*2d%*8s%*3c");
  scanf ("%*n", n); /* { dg-warning "suppress" "suppression of %n" } */
  scanf ("%*hd"); /* { dg-warning "together" "suppression with length" } */
  /* Valid, invalid and silly width constructions.  */
  scanf ("%2d%3i%4o%5u%6x%7X%8e%9E%10f%11g%12G%13s%14[abc]%15c%16p",
	 ip, ip, uip, uip, uip, uip, fp, fp, fp, fp, fp, s, s, s, pp);
  scanf ("%0d", ip); /* { dg-warning "width" "warning for zero width" } */
  scanf ("%3n", n); /* { dg-warning "width" "width with %n" } */
  /* Valid and invalid %h, %l, %L constructions.  */
  scanf ("%hd%hi%ho%hu%hx%hX%hn", hp, hp, uhp, uhp, uhp, uhp, hn);
  scanf ("%he", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hE", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hf", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hg", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hG", fp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hs", s); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%h[ac]", s); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hc", s); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%hp", pp); /* { dg-warning "length" "bad use of %h" } */
  scanf ("%h"); /* { dg-warning "conversion lacks type" "bare %h" } */
  scanf ("%h."); /* { dg-warning "conversion" "bogus %h" } */
  scanf ("%ld%li%lo%lu%lx%lX%ln", lp, lp, ulp, ulp, ulp, ulp, ln);
  scanf ("%le%lE%lf%lg%lG", dp, dp, dp, dp, dp);
  scanf ("%lp", pp); /* { dg-warning "length" "bad use of %l" } */
  /* These next three formats were added in C94.  */
  scanf ("%ls", ls); /* { dg-warning "length|C" "bad use of %l" } */
  scanf ("%l[ac]", ls); /* { dg-warning "length|C" "bad use of %l" } */
  scanf ("%lc", ls); /* { dg-warning "length|C" "bad use of %l" } */
  scanf ("%Le%LE%Lf%Lg%LG", ldp, ldp, ldp, ldp, ldp);
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
  scanf ("%d%i%o%u%x%X%e%E%f%g%G%s%[abc]%c%p%n%%", ip, ip, uip, uip, uip,
	 uip, fp, fp, fp, fp, fp, s, s, s, pp, n);
  /* Allow other character pointers with %s, %c, %[].  */
  scanf ("%2s%3s%4c%5c%6[abc]%7[abc]", ss, us, ss, us, ss, us);
  /* Further tests for %[].  */
  scanf ("%[%d]%d", s, ip);
  scanf ("%[^%d]%d", s, ip);
  scanf ("%[]%d]%d", s, ip);
  scanf ("%[^]%d]%d", s, ip);
  scanf ("%[%d]%d", s, ip);
  scanf ("%[]abcd", s); /* { dg-warning "no closing" "incomplete scanset" } */
  /* Various tests of bad argument types.  Some of these are only pedantic
     warnings.
  */
  scanf ("%d", lp); /* { dg-warning "format" "bad argument types" } */
  scanf ("%d", uip); /* { dg-warning "format" "bad argument types" } */
  scanf ("%d", pp); /* { dg-warning "format" "bad argument types" } */
  scanf ("%p", ppc); /* { dg-warning "format" "bad argument types" } */
  scanf ("%p", ppv); /* { dg-warning "format" "bad argument types" } */
  scanf ("%s", n); /* { dg-warning "format" "bad argument types" } */
  scanf ("%s", p); /* { dg-warning "format" "bad argument types" } */
  scanf ("%p", p); /* { dg-warning "format" "bad argument types" } */
  scanf ("%p", sp); /* { dg-warning "format" "bad argument types" } */
  /* Tests for writing into constant values.  */
  scanf ("%d", cip); /* { dg-warning "constant" "%d writing into const" } */
  scanf ("%n", cn); /* { dg-warning "constant" "%n writing into const" } */
  scanf ("%s", cs); /* { dg-warning "constant" "%s writing into const" } */
  scanf ("%p", pcp); /* { dg-warning "constant" "%p writing into const" } */
  /* Wrong number of arguments.  */
  scanf ("%d%d", ip); /* { dg-warning "arguments" "wrong number of args" } */
  scanf ("%d", ip, ip); /* { dg-warning "arguments" "wrong number of args" } */
  /* Miscellaneous bogus constructions.  */
  scanf (""); /* { dg-warning "zero-length" "warning for empty format" } */
  scanf ("\0"); /* { dg-warning "embedded" "warning for embedded NUL" } */
  scanf ("%d\0", ip); /* { dg-warning "embedded" "warning for embedded NUL" } */
  scanf ("%d\0%d", ip, ip); /* { dg-warning "embedded|too many" "warning for embedded NUL" } */
  scanf (NULL); /* { dg-warning "null" "null format string warning" } */
  scanf ("%"); /* { dg-warning "trailing" "trailing % warning" } */
  scanf ("%d", (int *)0); /* { dg-warning "null" "writing into NULL" } */
}
