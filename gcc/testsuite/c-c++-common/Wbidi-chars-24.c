/* PR preprocessor/103026 */
/* { dg-do compile } */
/* { dg-options "-Wbidi-chars=ucn,unpaired" } */
/* Test nesting of bidi chars in various contexts.  */

void
g1 ()
{
  const char *s1 = "a b c LRE\u{202a} 1 2 3 PDI\u{00000000000000000000000002069} x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s2 = "a b c RLE\u{00202b} 1 2 3 PDI\u{2069} x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s3 = "a b c LRO\u{000000202d} 1 2 3 PDI\u{02069} x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s4 = "a b c RLO\u{202e} 1 2 3 PDI\u{00000002069} x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s5 = "a b c LRI\u{002066} 1 2 3 PDF\u{202C} x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s6 = "a b c RLI\u{02067} 1 2 3 PDF\u{202c} x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s7 = "a b c FSI\u{0002068} 1 2 3 PDF\u{0202c} x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
}

int A\u{202a}B\u{2069}C;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int a\u{00000202b}B\u{000000002069}c;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
