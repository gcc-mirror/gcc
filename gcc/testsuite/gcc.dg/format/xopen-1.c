/* Test for X/Open format extensions, as found in the
   Single Unix Specification and in Austin Group draft 7.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

#include "format.h"

void
foo (int i, unsigned int u, wint_t lc, wchar_t *ls, int *ip, double d,
     char *s, void *p, int *n, long int l, int i2, float *fp, long int *lp,
     va_list va)
{
  /* The conversion specifiers C and S, for both printf and scanf,
     are X/Open extensions.
  */
  printf ("%C", lc);
  printf ("%3C", lc);
  printf ("%.3C", lc); /* { dg-warning "precision" "precision with %C" } */
  printf ("%hC", lc); /* { dg-warning "length" "bad %hC" } */
  printf ("%hhC", lc); /* { dg-warning "length" "bad %hhC" } */
  printf ("%lC", lc); /* { dg-warning "length" "bad %lC" } */
  printf ("%llC", lc); /* { dg-warning "length" "bad %llC" } */
  printf ("%jC", lc); /* { dg-warning "length" "bad %jC" } */
  printf ("%zC", lc); /* { dg-warning "length" "bad %zC" } */
  printf ("%tC", lc); /* { dg-warning "length" "bad %tC" } */
  printf ("%LC", lc); /* { dg-warning "length" "bad %LC" } */
  printf ("%-C", lc);
  printf ("%+C", lc); /* { dg-warning "flag" "bad %+C" } */
  printf ("% C", lc); /* { dg-warning "flag" "bad % C" } */
  printf ("%#C", lc); /* { dg-warning "flag" "bad %#C" } */
  printf ("%0C", lc); /* { dg-warning "flag" "bad %0C" } */
  printf ("%'C", lc); /* { dg-warning "flag" "bad %'C" } */
  printf ("%S", ls);
  printf ("%3S", ls);
  printf ("%.3S", ls);
  printf ("%hS", ls); /* { dg-warning "length" "bad %hS" } */
  printf ("%hhS", ls); /* { dg-warning "length" "bad %hhS" } */
  printf ("%lS", ls); /* { dg-warning "length" "bad %lS" } */
  printf ("%llS", ls); /* { dg-warning "length" "bad %llS" } */
  printf ("%jS", ls); /* { dg-warning "length" "bad %jS" } */
  printf ("%zS", ls); /* { dg-warning "length" "bad %zS" } */
  printf ("%tS", ls); /* { dg-warning "length" "bad %tS" } */
  printf ("%LS", ls); /* { dg-warning "length" "bad %LS" } */
  printf ("%-S", ls);
  printf ("%+S", ls); /* { dg-warning "flag" "bad %+S" } */
  printf ("% S", ls); /* { dg-warning "flag" "bad % S" } */
  printf ("%#S", ls); /* { dg-warning "flag" "bad %#S" } */
  printf ("%0S", ls); /* { dg-warning "flag" "bad %0S" } */
  printf ("%'S", ls); /* { dg-warning "flag" "bad %'S" } */
  scanf ("%C", ls);
  scanf ("%S", ls);
  scanf ("%*C%*S");
  scanf ("%2C%3S", ls, ls);
  scanf ("%hC", ls); /* { dg-warning "length" "bad %hC" } */
  scanf ("%hhC", ls); /* { dg-warning "length" "bad %hhC" } */
  scanf ("%lC", ls); /* { dg-warning "length" "bad %lC" } */
  scanf ("%llC", ls); /* { dg-warning "length" "bad %llC" } */
  scanf ("%jC", ls); /* { dg-warning "length" "bad %jC" } */
  scanf ("%zC", ls); /* { dg-warning "length" "bad %zC" } */
  scanf ("%tC", ls); /* { dg-warning "length" "bad %tC" } */
  scanf ("%LC", ls); /* { dg-warning "length" "bad %LC" } */
  scanf ("%hS", ls); /* { dg-warning "length" "bad %hS" } */
  scanf ("%hhS", ls); /* { dg-warning "length" "bad %hhS" } */
  scanf ("%lS", ls); /* { dg-warning "length" "bad %lS" } */
  scanf ("%llS", ls); /* { dg-warning "length" "bad %llS" } */
  scanf ("%jS", ls); /* { dg-warning "length" "bad %jS" } */
  scanf ("%zS", ls); /* { dg-warning "length" "bad %zS" } */
  scanf ("%tS", ls); /* { dg-warning "length" "bad %tS" } */
  scanf ("%LS", ls); /* { dg-warning "length" "bad %LS" } */
  /* In C99 mode (even with extensions), %aS is a floating point
     format followed by an S.
  */
  scanf ("%aS", fp);
  /* The printf flag character ' is an X/Open extension.  */
  printf ("%'d%'i%'u%'f%'F%'g%'G", i, i, u, d, d, d, d);
  printf ("%'o", u); /* { dg-warning "flag" "bad use of ' flag" } */
  printf ("%'x", u); /* { dg-warning "flag" "bad use of ' flag" } */
  printf ("%'X", u); /* { dg-warning "flag" "bad use of ' flag" } */
  printf ("%'e", d); /* { dg-warning "flag" "bad use of ' flag" } */
  printf ("%'E", d); /* { dg-warning "flag" "bad use of ' flag" } */
  printf ("%'a", d); /* { dg-warning "flag" "bad use of ' flag" } */
  printf ("%'A", d); /* { dg-warning "flag" "bad use of ' flag" } */
  printf ("%'c", i); /* { dg-warning "flag" "bad use of ' flag" } */
  printf ("%'s", s); /* { dg-warning "flag" "bad use of ' flag" } */
  printf ("%'p", p); /* { dg-warning "flag" "bad use of ' flag" } */
  printf ("%'n", n); /* { dg-warning "flag" "bad use of ' flag" } */
  /* The use of operand number $ formats is an X/Open extension.  */
  scanf ("%1$d", ip);
  printf ("%1$d", i);
  printf ("%1$d", l); /* { dg-warning "arg 2|argument 2" "mismatched args with $ format" } */
  printf ("%3$*2$.*1$ld", i2, i, l);
  printf ("%4$ld%7$ld%5$d%6$d%3$d%1$d%2$d", i, i, i, l, i, i, l);
  scanf ("%4$ld%7$ld%5$d%6$d%3$d%1$d%2$d", ip, ip, ip, lp, ip, ip, lp);
  printf ("%1$d%d", i, i); /* { dg-warning "missing" "mixing $ and non-$ formats" } */
  printf ("%%%1$d%%%2$d", i, i);
  printf ("%d%2$d", i); /* { dg-warning "used after format" "mixing $ and non-$ formats" } */
  printf ("%1$*d", i, i); /* { dg-warning "missing" "mixing $ and non-$ formats" } */
  printf ("%*1$d", i); /* { dg-warning "missing" "mixing $ and non-$ formats" } */
  scanf ("%1$d%d", ip, ip); /* { dg-warning "missing" "mixing $ and non-$ formats" } */
  scanf ("%*f%%%1$d%%%2$d", ip, ip);
  printf ("%2$d", i); /* { dg-warning "operand" "$ number too large" } */
  printf ("%0$d", i); /* { dg-warning "operand" "$ number too small" } */
  printf ("%3$d%1$d", i, i, i); /* { dg-warning "before used" "unused $ operand" } */
  printf ("%2$d%1$d", i, i, i); /* { dg-warning "unused" "unused $ operand" } */
  vprintf ("%3$d%1$d", va); /* { dg-warning "before used" "unused $ operand" } */
  /* With scanf formats, gaps in the used arguments are allowed only if the
     arguments are all pointers.  In such a case, should only give the lesser
     warning about unused arguments rather than the more serious one about
     argument gaps.  */
  scanf ("%3$d%1$d", ip, ip, ip); /* { dg-bogus "before used" "unused $ scanf pointer operand" } */
  /* { dg-warning "unused" "unused $ scanf pointer operand" { target *-*-* } .-1 } */
  /* If there are non-pointer arguments unused at the end, this is also OK.  */
  scanf ("%3$d%1$d", ip, ip, ip, i); /* { dg-bogus "before used" "unused $ scanf pointer operand" } */
  /* { dg-warning "unused" "unused $ scanf pointer operand" { target *-*-* } .-1 } */
  scanf ("%3$d%1$d", ip, i, ip); /* { dg-warning "before used" "unused $ scanf non-pointer operand" } */
  /* Can't check the arguments in the vscanf case, so should suppose the
     lesser problem.  */
  vscanf ("%3$d%1$d", va); /* { dg-bogus "before used" "unused $ scanf pointer operand" } */
  /* { dg-warning "unused" "unused $ scanf pointer operand" { target *-*-* } .-1 } */
  scanf ("%2$*d%1$d", ip, ip); /* { dg-warning "operand" "operand number with suppression" } */
  printf ("%1$d%1$d", i);
  scanf ("%1$d%1$d", ip); /* { dg-warning "more than once" "multiple use of scanf argument" } */
}
