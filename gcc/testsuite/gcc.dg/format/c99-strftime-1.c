/* Test for strftime formats.  Formats using C99 features.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic -Wformat -Wformat-y2k" } */

#include "format.h"

void
foo (char *s, size_t m, const struct tm *tp)
{
  /* See ISO/IEC 9899:1990 (E) subclause 7.12.3.5 (pages 174-175).  */
  /* Formats which are Y2K-compliant (no 2-digit years).  */
  strftime (s, m, "%a%A%b%B%C%d%e%F%G%h%H%I%j%m%M%p%R%S%t%T%u%U%V%w%W%X%Y%z%Z%%", tp);
  strftime (s, m, "%EC%EX%EY%Od%Oe%OH%OI%Om%OM%OS%Ou%OU%OV%Ow%OW", tp);
  /* Formats with 2-digit years.  */
  strftime (s, m, "%D", tp); /* { dg-warning "only last 2" "2-digit year" } */
  strftime (s, m, "%g", tp); /* { dg-warning "only last 2" "2-digit year" } */
  strftime (s, m, "%y", tp); /* { dg-warning "only last 2" "2-digit year" } */
  strftime (s, m, "%Oy", tp); /* { dg-warning "only last 2" "2-digit year" } */
  /* Formats with 2-digit years in some locales.  */
  strftime (s, m, "%c", tp); /* { dg-warning "some locales" "2-digit year" } */
  strftime (s, m, "%Ec", tp); /* { dg-warning "some locales" "2-digit year" } */
  strftime (s, m, "%x", tp); /* { dg-warning "some locales" "2-digit year" } */
  strftime (s, m, "%Ex", tp); /* { dg-warning "some locales" "2-digit year" } */
  /* %Ey is explicitly an era offset not a 2-digit year; but in some
     locales the E modifier may be ignored.
  */
  strftime (s, m, "%Ey", tp); /* { dg-warning "some locales" "2-digit year" } */
  /* Bad uses of %E and %O.  */
  strftime (s, m, "%EEY", tp); /* { dg-warning "multiple|repeated" "multiple %E/%O" } */
  strftime (s, m, "%EOy", tp); /* { dg-warning "multiple|together" "multiple %E/%O" } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } .-1 } */
  strftime (s, m, "%OEy", tp); /* { dg-warning "multiple|together" "multiple %E/%O" } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } .-1 } */
  strftime (s, m, "%OOV", tp); /* { dg-warning "multiple|repeated" "multiple %E/%O" } */
  strftime (s, m, "%Ea", tp); /* { dg-warning "flag|modifier" "bad %Ea" } */
  strftime (s, m, "%EA", tp); /* { dg-warning "flag|modifier" "bad %EA" } */
  strftime (s, m, "%Eb", tp); /* { dg-warning "flag|modifier" "bad %Eb" } */
  strftime (s, m, "%EB", tp); /* { dg-warning "flag|modifier" "bad %EB" } */
  strftime (s, m, "%Ed", tp); /* { dg-warning "flag|modifier" "bad %Ed" } */
  strftime (s, m, "%ED", tp); /* { dg-warning "flag|modifier" "bad %ED" } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } .-1 } */
  strftime (s, m, "%Ee", tp); /* { dg-warning "flag|modifier" "bad %Ee" } */
  strftime (s, m, "%EF", tp); /* { dg-warning "flag|modifier" "bad %EF" } */
  strftime (s, m, "%Eg", tp); /* { dg-warning "flag|modifier" "bad %Eg" } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } .-1 } */
  strftime (s, m, "%EG", tp); /* { dg-warning "flag|modifier" "bad %EG" } */
  strftime (s, m, "%Eh", tp); /* { dg-warning "flag|modifier" "bad %Eh" } */
  strftime (s, m, "%EH", tp); /* { dg-warning "flag|modifier" "bad %EH" } */
  strftime (s, m, "%EI", tp); /* { dg-warning "flag|modifier" "bad %EI" } */
  strftime (s, m, "%Ej", tp); /* { dg-warning "flag|modifier" "bad %Ej" } */
  strftime (s, m, "%Em", tp); /* { dg-warning "flag|modifier" "bad %Em" } */
  strftime (s, m, "%EM", tp); /* { dg-warning "flag|modifier" "bad %EM" } */
  strftime (s, m, "%En", tp); /* { dg-warning "flag|modifier" "bad %En" } */
  strftime (s, m, "%Ep", tp); /* { dg-warning "flag|modifier" "bad %Ep" } */
  strftime (s, m, "%Er", tp); /* { dg-warning "flag|modifier" "bad %Er" } */
  strftime (s, m, "%ER", tp); /* { dg-warning "flag|modifier" "bad %ER" } */
  strftime (s, m, "%ES", tp); /* { dg-warning "flag|modifier" "bad %ES" } */
  strftime (s, m, "%Et", tp); /* { dg-warning "flag|modifier" "bad %Et" } */
  strftime (s, m, "%ET", tp); /* { dg-warning "flag|modifier" "bad %ET" } */
  strftime (s, m, "%Eu", tp); /* { dg-warning "flag|modifier" "bad %Eu" } */
  strftime (s, m, "%EU", tp); /* { dg-warning "flag|modifier" "bad %EU" } */
  strftime (s, m, "%EV", tp); /* { dg-warning "flag|modifier" "bad %EV" } */
  strftime (s, m, "%Ew", tp); /* { dg-warning "flag|modifier" "bad %Ew" } */
  strftime (s, m, "%EW", tp); /* { dg-warning "flag|modifier" "bad %EW" } */
  strftime (s, m, "%Ez", tp); /* { dg-warning "flag|modifier" "bad %Ez" } */
  strftime (s, m, "%EZ", tp); /* { dg-warning "flag|modifier" "bad %EZ" } */
  strftime (s, m, "%E%", tp); /* { dg-warning "flag|modifier" "bad %E%" } */
  strftime (s, m, "%Oa", tp); /* { dg-warning "flag|modifier" "bad %Oa" } */
  strftime (s, m, "%OA", tp); /* { dg-warning "flag|modifier" "bad %OA" } */
  strftime (s, m, "%Ob", tp); /* { dg-warning "flag|modifier" "bad %Ob" } */
  strftime (s, m, "%OB", tp); /* { dg-warning "flag|modifier" "bad %OB" } */
  strftime (s, m, "%Oc", tp); /* { dg-warning "flag|modifier" "bad %Oc" } */
  /* { dg-warning "in some locales" "2-digit year" { target *-*-* } .-1 } */
  strftime (s, m, "%OC", tp); /* { dg-warning "flag|modifier|C" "bad %OC" } */
  strftime (s, m, "%OD", tp); /* { dg-warning "flag|modifier" "bad %OD" } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } .-1 } */
  strftime (s, m, "%OF", tp); /* { dg-warning "flag|modifier" "bad %OF" } */
  strftime (s, m, "%Og", tp); /* { dg-warning "flag|modifier|C" "bad %Og" } */
  /* { dg-warning "only last 2" "2-digit year" { target *-*-* } .-1 } */
  strftime (s, m, "%OG", tp); /* { dg-warning "flag|modifier|C" "bad %OG" } */
  strftime (s, m, "%Oh", tp); /* { dg-warning "flag|modifier" "bad %Oh" } */
  strftime (s, m, "%Oj", tp); /* { dg-warning "flag|modifier|C" "bad %Oj" } */
  strftime (s, m, "%On", tp); /* { dg-warning "flag|modifier" "bad %On" } */
  strftime (s, m, "%Op", tp); /* { dg-warning "flag|modifier" "bad %Op" } */
  strftime (s, m, "%Or", tp); /* { dg-warning "flag|modifier" "bad %Or" } */
  strftime (s, m, "%OR", tp); /* { dg-warning "flag|modifier" "bad %OR" } */
  strftime (s, m, "%Ot", tp); /* { dg-warning "flag|modifier" "bad %Ot" } */
  strftime (s, m, "%OT", tp); /* { dg-warning "flag|modifier" "bad %OT" } */
  strftime (s, m, "%Ox", tp); /* { dg-warning "flag|modifier" "bad %Ox" } */
  /* { dg-warning "in some locales" "2-digit year" { target *-*-* } .-1 } */
  strftime (s, m, "%OX", tp); /* { dg-warning "flag|modifier" "bad %OX" } */
  strftime (s, m, "%OY", tp); /* { dg-warning "flag|modifier|C" "bad %OY" } */
  strftime (s, m, "%Oz", tp); /* { dg-warning "flag|modifier|C" "bad %Oz" } */
  strftime (s, m, "%OZ", tp); /* { dg-warning "flag|modifier" "bad %OZ" } */
  strftime (s, m, "%O%", tp); /* { dg-warning "flag|modifier" "bad %O%" } */
}
