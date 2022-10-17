/* PR preprocessor/103026 */
/* { dg-do compile } */
/* { dg-options "-Wbidi-chars=any,ucn -Wno-multichar -Wno-overflow" } */
/* Test all bidi chars in various contexts (identifiers, comments,
   string literals, character constants), both UCN and UTF-8.  The bidi
   chars here are properly terminated, except for the character constants.  */

/* a b c LRE‪ 1 2 3 PDF‬ x y z */
/* { dg-warning "U\\+202A" "" { target *-*-* } .-1 } */
/* a b c RLE‫ 1 2 3 PDF‬ x y z */
/* { dg-warning "U\\+202B" "" { target *-*-* } .-1 } */
/* a b c LRO‭ 1 2 3 PDF‬ x y z */
/* { dg-warning "U\\+202D" "" { target *-*-* } .-1 } */
/* a b c RLO‮ 1 2 3 PDF‬ x y z */
/* { dg-warning "U\\+202E" "" { target *-*-* } .-1 } */
/* a b c LRI⁦ 1 2 3 PDI⁩ x y z */
/* { dg-warning "U\\+2066" "" { target *-*-* } .-1 } */
/* a b c RLI⁧ 1 2 3 PDI⁩ x y */
/* { dg-warning "U\\+2067" "" { target *-*-* } .-1 } */
/* a b c FSI⁨ 1 2 3 PDI⁩ x y z */
/* { dg-warning "U\\+2068" "" { target *-*-* } .-1 } */

/* Same but C++ comments instead.  */
// a b c LRE‪ 1 2 3 PDF‬ x y z
/* { dg-warning "U\\+202A" "" { target *-*-* } .-1 } */
// a b c RLE‫ 1 2 3 PDF‬ x y z
/* { dg-warning "U\\+202B" "" { target *-*-* } .-1 } */
// a b c LRO‭ 1 2 3 PDF‬ x y z
/* { dg-warning "U\\+202D" "" { target *-*-* } .-1 } */
// a b c RLO‮ 1 2 3 PDF‬ x y z
/* { dg-warning "U\\+202E" "" { target *-*-* } .-1 } */
// a b c LRI⁦ 1 2 3 PDI⁩ x y z
/* { dg-warning "U\\+2066" "" { target *-*-* } .-1 } */
// a b c RLI⁧ 1 2 3 PDI⁩ x y
/* { dg-warning "U\\+2067" "" { target *-*-* } .-1 } */
// a b c FSI⁨ 1 2 3 PDI⁩ x y z
/* { dg-warning "U\\+2068" "" { target *-*-* } .-1 } */

/* Here we're closing an unopened context, warn when =any.  */
/* a b c PDI⁩ x y z */
/* { dg-warning "U\\+2069" "" { target *-*-* } .-1 } */
/* a b c PDF‬ x y z */
/* { dg-warning "U\\+202C" "" { target *-*-* } .-1 } */
// a b c PDI⁩ x y z
/* { dg-warning "U\\+2069" "" { target *-*-* } .-1 } */
// a b c PDF‬ x y z
/* { dg-warning "U\\+202C" "" { target *-*-* } .-1 } */

/* Multiline comments.  */
/* a b c PDI⁩ x y z
   */
/* { dg-warning "U\\+2069" "" { target *-*-* } .-2 } */
/* a b c PDF‬ x y z
   */
/* { dg-warning "U\\+202C" "" { target *-*-* } .-2 } */
/* first
   a b c PDI⁩ x y z
   */
/* { dg-warning "U\\+2069" "" { target *-*-* } .-2 } */
/* first
   a b c PDF‬ x y z
   */
/* { dg-warning "U\\+202C" "" { target *-*-* } .-2 } */
/* first
   a b c PDI⁩ x y z */
/* { dg-warning "U\\+2069" "" { target *-*-* } .-1 } */
/* first
   a b c PDF‬ x y z */
/* { dg-warning "U\\+202C" "" { target *-*-* } .-1 } */

void
g1 ()
{
  const char *s1 = "a b c LRE‪ 1 2 3 PDF‬ x y z";
/* { dg-warning "U\\+202A" "" { target *-*-* } .-1 } */
  const char *s2 = "a b c RLE‫ 1 2 3 PDF‬ x y z";
/* { dg-warning "U\\+202B" "" { target *-*-* } .-1 } */
  const char *s3 = "a b c LRO‭ 1 2 3 PDF‬ x y z";
/* { dg-warning "U\\+202D" "" { target *-*-* } .-1 } */
  const char *s4 = "a b c RLO‮ 1 2 3 PDF‬ x y z";
/* { dg-warning "U\\+202E" "" { target *-*-* } .-1 } */
  const char *s5 = "a b c LRI⁦ 1 2 3 PDI⁩ x y z";
/* { dg-warning "U\\+2066" "" { target *-*-* } .-1 } */
  const char *s6 = "a b c RLI⁧ 1 2 3 PDI⁩ x y z";
/* { dg-warning "U\\+2067" "" { target *-*-* } .-1 } */
  const char *s7 = "a b c FSI⁨ 1 2 3 PDI⁩ x y z";
/* { dg-warning "U\\+2068" "" { target *-*-* } .-1 } */
  const char *s8 = "a b c PDI⁩ x y z";
/* { dg-warning "U\\+2069" "" { target *-*-* } .-1 } */
  const char *s9 = "a b c PDF‬ x y z";
/* { dg-warning "U\\+202C" "" { target *-*-* } .-1 } */

  const char *s10 = "a b c LRE\u202a 1 2 3 PDF\u202c x y z";
/* { dg-warning "U\\+202A" "" { target *-*-* } .-1 } */
  const char *s11 = "a b c LRE\u202A 1 2 3 PDF\u202c x y z";
/* { dg-warning "U\\+202A" "" { target *-*-* } .-1 } */
  const char *s12 = "a b c RLE\u202b 1 2 3 PDF\u202c x y z";
/* { dg-warning "U\\+202B" "" { target *-*-* } .-1 } */
  const char *s13 = "a b c RLE\u202B 1 2 3 PDF\u202c x y z";
/* { dg-warning "U\\+202B" "" { target *-*-* } .-1 } */
  const char *s14 = "a b c LRO\u202d 1 2 3 PDF\u202c x y z";
/* { dg-warning "U\\+202D" "" { target *-*-* } .-1 } */
  const char *s15 = "a b c LRO\u202D 1 2 3 PDF\u202c x y z";
/* { dg-warning "U\\+202D" "" { target *-*-* } .-1 } */
  const char *s16 = "a b c RLO\u202e 1 2 3 PDF\u202c x y z";
/* { dg-warning "U\\+202E" "" { target *-*-* } .-1 } */
  const char *s17 = "a b c RLO\u202E 1 2 3 PDF\u202c x y z";
/* { dg-warning "U\\+202E" "" { target *-*-* } .-1 } */
  const char *s18 = "a b c LRI\u2066 1 2 3 PDI\u2069 x y z";
/* { dg-warning "U\\+2066" "" { target *-*-* } .-1 } */
  const char *s19 = "a b c RLI\u2067 1 2 3 PDI\u2069 x y z";
/* { dg-warning "U\\+2067" "" { target *-*-* } .-1 } */
  const char *s20 = "a b c FSI\u2068 1 2 3 PDI\u2069 x y z";
/* { dg-warning "U\\+2068" "" { target *-*-* } .-1 } */
}

void
g2 ()
{
  const char c1 = '\u202a';
/* { dg-warning "U\\+202A" "" { target *-*-* } .-1 } */
  const char c2 = '\u202A';
/* { dg-warning "U\\+202A" "" { target *-*-* } .-1 } */
  const char c3 = '\u202b';
/* { dg-warning "U\\+202B" "" { target *-*-* } .-1 } */
  const char c4 = '\u202B';
/* { dg-warning "U\\+202B" "" { target *-*-* } .-1 } */
  const char c5 = '\u202d';
/* { dg-warning "U\\+202D" "" { target *-*-* } .-1 } */
  const char c6 = '\u202D';
/* { dg-warning "U\\+202D" "" { target *-*-* } .-1 } */
  const char c7 = '\u202e';
/* { dg-warning "U\\+202E" "" { target *-*-* } .-1 } */
  const char c8 = '\u202E';
/* { dg-warning "U\\+202E" "" { target *-*-* } .-1 } */
  const char c9 = '\u2066';
/* { dg-warning "U\\+2066" "" { target *-*-* } .-1 } */
  const char c10 = '\u2067';
/* { dg-warning "U\\+2067" "" { target *-*-* } .-1 } */
  const char c11 = '\u2068';
/* { dg-warning "U\\+2068" "" { target *-*-* } .-1 } */
}

int a‪b‬c;
/* { dg-warning "U\\+202A" "" { target *-*-* } .-1 } */
int a‫b‬c;
/* { dg-warning "U\\+202B" "" { target *-*-* } .-1 } */
int a‭b‬c;
/* { dg-warning "U\\+202D" "" { target *-*-* } .-1 } */
int a‮b‬c;
/* { dg-warning "U\\+202E" "" { target *-*-* } .-1 } */
int a⁦b⁩c;
/* { dg-warning "U\\+2066" "" { target *-*-* } .-1 } */
int a⁧b⁩c;
/* { dg-warning "U\\+2067" "" { target *-*-* } .-1 } */
int a⁨b⁩c;
/* { dg-warning "U\\+2068" "" { target *-*-* } .-1 } */
int A‬X;
/* { dg-warning "U\\+202C" "" { target *-*-* } .-1 } */
int A\u202cY;
/* { dg-warning "U\\+202C" "" { target *-*-* } .-1 } */
int A\u202CY2;
/* { dg-warning "U\\+202C" "" { target *-*-* } .-1 } */

int d\u202ae\u202cf;
/* { dg-warning "U\\+202A" "" { target *-*-* } .-1 } */
int d\u202Ae\u202cf2;
/* { dg-warning "U\\+202A" "" { target *-*-* } .-1 } */
int d\u202be\u202cf;
/* { dg-warning "U\\+202B" "" { target *-*-* } .-1 } */
int d\u202Be\u202cf2;
/* { dg-warning "U\\+202B" "" { target *-*-* } .-1 } */
int d\u202de\u202cf;
/* { dg-warning "U\\+202D" "" { target *-*-* } .-1 } */
int d\u202De\u202cf2;
/* { dg-warning "U\\+202D" "" { target *-*-* } .-1 } */
int d\u202ee\u202cf;
/* { dg-warning "U\\+202E" "" { target *-*-* } .-1 } */
int d\u202Ee\u202cf2;
/* { dg-warning "U\\+202E" "" { target *-*-* } .-1 } */
int d\u2066e\u2069f;
/* { dg-warning "U\\+2066" "" { target *-*-* } .-1 } */
int d\u2067e\u2069f;
/* { dg-warning "U\\+2067" "" { target *-*-* } .-1 } */
int d\u2068e\u2069f;
/* { dg-warning "U\\+2068" "" { target *-*-* } .-1 } */
int X\u2069;
/* { dg-warning "U\\+2069" "" { target *-*-* } .-1 } */
