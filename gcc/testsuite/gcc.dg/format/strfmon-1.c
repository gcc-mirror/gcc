/* Test for strfmon format checking.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wformat" } */

#include "format.h"

void
foo (char *s, size_t m, double d, long double ld)
{
  /* Examples of valid formats from Austin Group draft 7.  */
  strfmon (s, m, "%n", d);
  strfmon (s, m, "%11n", d);
  strfmon (s, m, "%#5n", d);
  strfmon (s, m, "%=*#5n", d);
  strfmon (s, m, "%=0#5n", d);
  strfmon (s, m, "%^#5n", d);
  strfmon (s, m, "%^#5.0n", d);
  strfmon (s, m, "%^#5.4n", d);
  strfmon (s, m, "%(#5n", d);
  strfmon (s, m, "%!(#5n", d);
  strfmon (s, m, "%-14#5.4n", d);
  strfmon (s, m, "%14#5.4n", d);
  /* Some more valid formats, including the GNU L length extension.  */
  strfmon (s, m, "abc%-11ndef%==i%%", d, d);
  strfmon (s, m, "%%abc%-11ndef%==Li%=%i", d, ld, d);
  strfmon (s, m, "%Li", ld);
  strfmon (s, m, "%11Li", ld);
  strfmon (s, m, "%#5Li", ld);
  strfmon (s, m, "%=*#5Li", ld);
  strfmon (s, m, "%=0#5Li", ld);
  strfmon (s, m, "%^#5Li", ld);
  strfmon (s, m, "%^#5.0Li", ld);
  strfmon (s, m, "%^#5.4Li", ld);
  strfmon (s, m, "%(#5Li", ld);
  strfmon (s, m, "%!(#5Li", ld);
  strfmon (s, m, "%-14#5.4Li", ld);
  strfmon (s, m, "%14#5.4Li", ld);
  /* Formats with the wrong types used.  */
  strfmon (s, m, "%Ln", d); /* { dg-warning "format" "wrong type" } */
  strfmon (s, m, "%n", ld); /* { dg-warning "format" "wrong type" } */
  /* The + and ( flags cannot be used together.  */
  strfmon (s, m, "%+(i", d); /* { dg-warning "flag" "+ and ( flags" } */
  strfmon (s, m, "%(+i", d); /* { dg-warning "flag" "+ and ( flags" } */
  /* Although empty precision is OK for printf, it isn't here.  */
  strfmon (s, m, "%#.5n", d); /* { dg-warning "empty" "empty left precision" } */
  strfmon (s, m, "%#5.n", d); /* { dg-warning "empty" "empty right precision" } */
  /* However, zero is a valid value for width and precisions.  */
  strfmon (s, m, "%0#0.0n", d);
  /* Test bogus %% constructions.  */
  strfmon (s, m, "%^%"); /* { dg-warning "format" "bogus %%" } */
  strfmon (s, m, "%!%\n"); /* { dg-warning "format" "bogus %%" } */
  strfmon (s, m, "%5%\n"); /* { dg-warning "format" "bogus %%" } */
  strfmon (s, m, "%.5%\n"); /* { dg-warning "format" "bogus %%" } */
  strfmon (s, m, "%#5%\n"); /* { dg-warning "format" "bogus %%" } */
  /* Miscellaneous bogus formats.  */
  strfmon (s, m, "%n%n", d); /* { dg-warning "arguments" "too few args" } */
  strfmon (s, m, ""); /* { dg-warning "zero-length" "empty" } */
  strfmon (s, m, NULL); /* { dg-warning "null" "null format string" } */
  strfmon (s, m, "%"); /* { dg-warning "trailing" "tailing %" } */
  strfmon (s, m, "%n\0", d); /* { dg-warning "embedded" "embedded NUL" } */
  strfmon (s, m, "%^^n", d); /* { dg-warning "repeated" "repeated flag" } */
}
