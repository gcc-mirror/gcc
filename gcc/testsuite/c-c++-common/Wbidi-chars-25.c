/* PR preprocessor/103026 */
/* { dg-do compile } */
/* { dg-options "-Wbidi-chars=ucn,unpaired" } */
/* Test nesting of bidi chars in various contexts.  */

void
g1 ()
{
  const char *s1 = "a b c LRE\N{LEFT-TO-RIGHT EMBEDDING} 1 2 3 PDI\N{POP DIRECTIONAL ISOLATE} x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s2 = "a b c RLE\N{RIGHT-TO-LEFT EMBEDDING} 1 2 3 PDI\N{POP DIRECTIONAL ISOLATE} x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s3 = "a b c LRO\N{LEFT-TO-RIGHT OVERRIDE} 1 2 3 PDI\N{POP DIRECTIONAL ISOLATE} x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s4 = "a b c RLO\N{RIGHT-TO-LEFT OVERRIDE} 1 2 3 PDI\N{POP DIRECTIONAL ISOLATE} x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s5 = "a b c LRI\N{LEFT-TO-RIGHT ISOLATE} 1 2 3 PDF\N{POP DIRECTIONAL FORMATTING} x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s6 = "a b c RLI\N{RIGHT-TO-LEFT ISOLATE} 1 2 3 PDF\N{POP DIRECTIONAL FORMATTING} x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
  const char *s7 = "a b c FSI\N{FIRST STRONG ISOLATE} 1 2 3 PDF\N{POP DIRECTIONAL FORMATTING} x y z";
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
}

int A\N{LEFT-TO-RIGHT EMBEDDING}B\N{POP DIRECTIONAL ISOLATE}C;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
int a\N{RIGHT-TO-LEFT EMBEDDING}B\N{POP DIRECTIONAL ISOLATE}c;
/* { dg-warning "unpaired" "" { target *-*-* } .-1 } */
