/* Test for printf formats.  Formats using C90 features, including cases
   where C90 specifies some aspect of the format to be ignored or where
   the behavior is undefined.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic -Wformat" } */

#include "format.h"

void
foo (int i, int i1, int i2, unsigned int u, double d, char *s, void *p,
     int *n, short int *hn, long int l, unsigned long int ul,
     long int *ln, long double ld, wint_t lc, wchar_t *ls, llong ll,
     ullong ull, unsigned int *un, const int *cn, signed char *ss,
     unsigned char *us, const signed char *css, unsigned int u1,
     unsigned int u2)
{
  /* See ISO/IEC 9899:1990 (E) subclause 7.9.6.1 (pages 131-134).  */
  /* Basic sanity checks for the different components of a format.  */
  printf ("%d\n", i);
  printf ("%+d\n", i);
  printf ("%3d\n", i);
  printf ("%-3d\n", i);
  printf ("%.7d\n", i);
  printf ("%+9.4d\n", i);
  printf ("%.3ld\n", l);
  printf ("%*d\n", i1, i);
  printf ("%.*d\n", i2, i);
  printf ("%*.*ld\n", i1, i2, l);
  printf ("%d %lu\n", i, ul);
  /* GCC has objected to the next one in the past, but it is a valid way
     of specifying zero precision.
  */
  printf ("%.e\n", d); /* { dg-bogus "precision" "bogus precision warning" } */
  /* Bogus use of width.  */
  printf ("%5n\n", n); /* { dg-warning "width" "width with %n" } */
  /* Erroneous, ignored or pointless constructs with precision.  */
  /* Whether negative values for precision may be included in the format
     string is not entirely clear; presume not, following Clive Feather's
     proposed resolution to DR#220 against C99.  In any case, such a
     construct should be warned about.
  */
  printf ("%.-5d\n", i); /* { dg-warning "format|precision" "negative precision warning" } */
  printf ("%.-*d\n", i); /* { dg-warning "format" "broken %.-*d format" } */
  printf ("%.3c\n", i); /* { dg-warning "precision" "precision with %c" } */
  printf ("%.3p\n", p); /* { dg-warning "precision" "precision with %p" } */
  printf ("%.3n\n", n); /* { dg-warning "precision" "precision with %n" } */
  /* Valid and invalid %% constructions.  Some of the warning messages
     are non-optimal, but they do detect the errorneous nature of the
     format string.
  */
  printf ("%%");
  printf ("%.3%"); /* { dg-warning "format" "bogus %%" } */
  printf ("%-%"); /* { dg-warning "format" "bogus %%" } */
  printf ("%-%\n"); /* { dg-warning "format" "bogus %%" } */
  printf ("%5%\n"); /* { dg-warning "format" "bogus %%" } */
  printf ("%h%\n"); /* { dg-warning "format" "bogus %%" } */
  /* Valid and invalid %h, %l, %L constructions.  */
  printf ("%hd", i);
  printf ("%hi", i);
  /* Strictly, these parameters should be int or unsigned int according to
     what unsigned short promotes to.  However, GCC ignores sign
     differences in format checking here, and this is relied on to get the
     correct checking without print_char_table needing to know whether
     int and short are the same size.
  */
  printf ("%ho%hu%hx%hX", u, u, u, u);
  printf ("%hn", hn);
  printf ("%hf", d); /* { dg-warning "length" "bad use of %h" } */
  printf ("%he", d); /* { dg-warning "length" "bad use of %h" } */
  printf ("%hE", d); /* { dg-warning "length" "bad use of %h" } */
  printf ("%hg", d); /* { dg-warning "length" "bad use of %h" } */
  printf ("%hG", d); /* { dg-warning "length" "bad use of %h" } */
  printf ("%hc", i); /* { dg-warning "length" "bad use of %h" } */
  printf ("%hs", s); /* { dg-warning "length" "bad use of %h" } */
  printf ("%hp", p); /* { dg-warning "length" "bad use of %h" } */
  printf ("%h"); /* { dg-warning "conversion lacks type" "bare %h" } */
  printf ("%h."); /* { dg-warning "conversion" "bogus %h." } */
  printf ("%ld%li%lo%lu%lx%lX", l, l, ul, ul, ul, ul);
  printf ("%ln", ln);
  printf ("%lf", d); /* { dg-warning "length|C" "bad use of %l" } */
  printf ("%le", d); /* { dg-warning "length|C" "bad use of %l" } */
  printf ("%lE", d); /* { dg-warning "length|C" "bad use of %l" } */
  printf ("%lg", d); /* { dg-warning "length|C" "bad use of %l" } */
  printf ("%lG", d); /* { dg-warning "length|C" "bad use of %l" } */
  printf ("%lp", p); /* { dg-warning "length|C" "bad use of %l" } */
  /* These next two were added in C94, but should be objected to in C90.
     For the first one, GCC has wanted wchar_t instead of the correct C94
     and C99 wint_t.
  */
  printf ("%lc", lc); /* { dg-warning "length|C" "C90 bad use of %l" } */
  printf ("%ls", ls); /* { dg-warning "length|C" "C90 bad use of %l" } */
  /* These uses of %L are legitimate, though GCC has wrongly warned for
     them in the past.
  */
  printf ("%Le%LE%Lf%Lg%LG", ld, ld, ld, ld, ld);
  /* These next six are accepted by GCC as referring to long long,
     but -pedantic correctly warns.
  */
  printf ("%Ld", ll); /* { dg-warning "does not support" "bad use of %L" } */
  printf ("%Li", ll); /* { dg-warning "does not support" "bad use of %L" } */
  printf ("%Lo", ull); /* { dg-warning "does not support" "bad use of %L" } */
  printf ("%Lu", ull); /* { dg-warning "does not support" "bad use of %L" } */
  printf ("%Lx", ull); /* { dg-warning "does not support" "bad use of %L" } */
  printf ("%LX", ull); /* { dg-warning "does not support" "bad use of %L" } */
  printf ("%Lc", i); /* { dg-warning "length" "bad use of %L" } */
  printf ("%Ls", s); /* { dg-warning "length" "bad use of %L" } */
  printf ("%Lp", p); /* { dg-warning "length" "bad use of %L" } */
  printf ("%Ln", n); /* { dg-warning "length" "bad use of %L" } */
  /* Valid uses of each bare conversion.  */
  printf ("%d%i%o%u%x%X%f%e%E%g%G%c%s%p%n%%", i, i, u, u, u, u, d, d, d, d, d,
	  i, s, p, n);
  /* Uses of the - flag (valid on all non-%, non-n conversions).  */
  printf ("%-d%-i%-o%-u%-x%-X%-f%-e%-E%-g%-G%-c%-s%-p", i, i, u, u, u, u,
	  d, d, d, d, d, i, s, p);
  printf ("%-n", n); /* { dg-warning "flag" "bad use of %-n" } */
  /* Uses of the + flag (valid on signed conversions only).  */
  printf ("%+d%+i%+f%+e%+E%+g%+G\n", i, i, d, d, d, d, d);
  printf ("%+o", u); /* { dg-warning "flag" "bad use of + flag" } */
  printf ("%+u", u); /* { dg-warning "flag" "bad use of + flag" } */
  printf ("%+x", u); /* { dg-warning "flag" "bad use of + flag" } */
  printf ("%+X", u); /* { dg-warning "flag" "bad use of + flag" } */
  printf ("%+c", i); /* { dg-warning "flag" "bad use of + flag" } */
  printf ("%+s", s); /* { dg-warning "flag" "bad use of + flag" } */
  printf ("%+p", p); /* { dg-warning "flag" "bad use of + flag" } */
  printf ("%+n", n); /* { dg-warning "flag" "bad use of + flag" } */
  /* Uses of the space flag (valid on signed conversions only, and ignored
     with +).
  */
  printf ("% +d", i); /* { dg-warning "use of both|ignored" "use of space and + flags" } */
  printf ("%+ d", i); /* { dg-warning "use of both|ignored" "use of space and + flags" } */
  printf ("% d% i% f% e% E% g% G\n", i, i, d, d, d, d, d);
  printf ("% o", u); /* { dg-warning "flag" "bad use of space flag" } */
  printf ("% u", u); /* { dg-warning "flag" "bad use of space flag" } */
  printf ("% x", u); /* { dg-warning "flag" "bad use of space flag" } */
  printf ("% X", u); /* { dg-warning "flag" "bad use of space flag" } */
  printf ("% c", i); /* { dg-warning "flag" "bad use of space flag" } */
  printf ("% s", s); /* { dg-warning "flag" "bad use of space flag" } */
  printf ("% p", p); /* { dg-warning "flag" "bad use of space flag" } */
  printf ("% n", n); /* { dg-warning "flag" "bad use of space flag" } */
  /* Uses of the # flag.  */
  printf ("%#o%#x%#X%#e%#E%#f%#g%#G", u, u, u, d, d, d, d, d);
  printf ("%#d", i); /* { dg-warning "flag" "bad use of # flag" } */
  printf ("%#i", i); /* { dg-warning "flag" "bad use of # flag" } */
  printf ("%#u", u); /* { dg-warning "flag" "bad use of # flag" } */
  printf ("%#c", i); /* { dg-warning "flag" "bad use of # flag" } */
  printf ("%#s", s); /* { dg-warning "flag" "bad use of # flag" } */
  printf ("%#p", p); /* { dg-warning "flag" "bad use of # flag" } */
  printf ("%#n", n); /* { dg-warning "flag" "bad use of # flag" } */
  /* Uses of the 0 flag.  */
  printf ("%08d%08i%08o%08u%08x%08X%08e%08E%08f%08g%08G", i, i, u, u, u, u,
	  d, d, d, d, d);
  printf ("%0c", i); /* { dg-warning "flag" "bad use of 0 flag" } */
  printf ("%0s", s); /* { dg-warning "flag" "bad use of 0 flag" } */
  printf ("%0p", p); /* { dg-warning "flag" "bad use of 0 flag" } */
  printf ("%0n", n); /* { dg-warning "flag" "bad use of 0 flag" } */
  /* 0 flag ignored with precision for certain types, not others.  */
  printf ("%08.5d", i); /* { dg-warning "ignored" "0 flag ignored with precision" } */
  printf ("%08.5i", i); /* { dg-warning "ignored" "0 flag ignored with precision" } */
  printf ("%08.5o", u); /* { dg-warning "ignored" "0 flag ignored with precision" } */
  printf ("%08.5u", u); /* { dg-warning "ignored" "0 flag ignored with precision" } */
  printf ("%08.5x", u); /* { dg-warning "ignored" "0 flag ignored with precision" } */
  printf ("%08.5X", u); /* { dg-warning "ignored" "0 flag ignored with precision" } */
  printf ("%08.5f%08.5e%08.5E%08.5g%08.5G", d, d, d, d, d);
  /* 0 flag ignored with - flag.  */
  printf ("%-08d", i); /* { dg-warning "flags|ignored" "0 flag ignored with - flag" } */
  printf ("%-08i", i); /* { dg-warning "flags|ignored" "0 flag ignored with - flag" } */
  printf ("%-08o", u); /* { dg-warning "flags|ignored" "0 flag ignored with - flag" } */
  printf ("%-08u", u); /* { dg-warning "flags|ignored" "0 flag ignored with - flag" } */
  printf ("%-08x", u); /* { dg-warning "flags|ignored" "0 flag ignored with - flag" } */
  printf ("%-08X", u); /* { dg-warning "flags|ignored" "0 flag ignored with - flag" } */
  printf ("%-08e", d); /* { dg-warning "flags|ignored" "0 flag ignored with - flag" } */
  printf ("%-08E", d); /* { dg-warning "flags|ignored" "0 flag ignored with - flag" } */
  printf ("%-08f", d); /* { dg-warning "flags|ignored" "0 flag ignored with - flag" } */
  printf ("%-08g", d); /* { dg-warning "flags|ignored" "0 flag ignored with - flag" } */
  printf ("%-08G", d); /* { dg-warning "flags|ignored" "0 flag ignored with - flag" } */
  /* Various tests of bad argument types.  */
  printf ("%d", l); /* { dg-warning "format" "bad argument types" } */
  printf ("%*.*d", l, i2, i); /* { dg-warning "field" "bad * argument types" } */
  printf ("%*.*d", i1, l, i); /* { dg-warning "field" "bad * argument types" } */
  printf ("%ld", i); /* { dg-warning "format" "bad argument types" } */
  printf ("%s", n); /* { dg-warning "format" "bad argument types" } */
  printf ("%p", i); /* { dg-warning "format" "bad argument types" } */
  printf ("%n", p); /* { dg-warning "format" "bad argument types" } */
  /* With -pedantic, we want some further checks for pointer targets:
     %p should allow only pointers to void (possibly qualified) and
     to character types (possibly qualified), but not function pointers
     or pointers to other types.  (Whether, in fact, character types are
     allowed here is unclear; see thread on comp.std.c, July 2000 for
     discussion of the requirements of rules on identical representation,
     and of the application of the as if rule with the new va_arg
     allowances in C99 to printf.)  Likewise, we should warn if
     pointer targets differ in signedness, except in some circumstances
     for character pointers.  (In C99 we should consider warning for
     char * or unsigned char * being passed to %hhn, even if strictly
     legitimate by the standard.)
  */
  printf ("%p", foo); /* { dg-warning "format" "bad argument types" } */
  printf ("%n", un); /* { dg-warning "format" "bad argument types" } */
  printf ("%p", n); /* { dg-warning "format" "bad argument types" } */
  /* Allow character pointers with %p.  */
  printf ("%p%p%p%p", s, ss, us, css);
  /* %s allows any character type.  */
  printf ("%s%s%s%s", s, ss, us, css);
  /* Warning for void * arguments for %s is GCC's historical behavior,
     and seems useful to keep, even if some standard versions might be
     read to permit it.
  */
  printf ("%s", p); /* { dg-warning "format" "bad argument types" } */
  /* The historical behavior is to allow signed / unsigned types
     interchangeably as arguments.  For values representable in both types,
     such usage may be correct.  For now preserve the behavior of GCC
     in such cases.
  */
  printf ("%d", u);
  /* Also allow the same for width and precision arguments.  In the past,
     GCC has been inconsistent and allowed unsigned for width but not
     precision.
  */
  printf ("%*.*d", u1, u2, i);
  /* Wrong number of arguments.  */
  printf ("%d%d", i); /* { dg-warning "matching" "wrong number of args" } */
  printf ("%d", i, i); /* { dg-warning "arguments" "wrong number of args" } */
  /* Miscellaneous bogus constructions.  */
  printf (""); /* { dg-warning "zero-length" "warning for empty format" } */
  printf ("\0"); /* { dg-warning "embedded" "warning for embedded NUL" } */
  printf ("%d\0", i); /* { dg-warning "embedded" "warning for embedded NUL" } */
  printf ("%d\0%d", i, i); /* { dg-warning "embedded|too many" "warning for embedded NUL" } */
  printf (NULL); /* { dg-warning "null" "null format string warning" } */
  printf ("%"); /* { dg-warning "trailing" "trailing % warning" } */
  printf ("%++d", i); /* { dg-warning "repeated" "repeated flag warning" } */
  printf ("%n", cn); /* { dg-warning "constant" "%n with const" } */
  printf ((const char *)L"foo"); /* { dg-warning "wide" "wide string" } */
  printf ("%n", (int *)0); /* { dg-warning "null" "%n with NULL" } */
  printf ("%s", (char *)0); /* { dg-warning "null" "%s with NULL" } */
}
