/* P2071R2 - Named universal character escapes */
/* { dg-do compile } */
/* { dg-require-effective-target wchar } */
/* { dg-options "-std=c17 -Wno-c++-compat" { target c } } */
/* { dg-options "-std=c++20" { target c++ } } */

#define z(x) 0
#define a z(
int b = a\N{});
int c = a\N{);
int d = a\N);
int e = a\NARG);
int f = a\N{abc});
int g = a\N{ABC.123});
int h = a\N{NON-EXISTENT CHAR});	/* { dg-bogus "is not a valid universal character" } */
int i = a\N{Latin_Small_Letter_A_With_Acute});
int j = a\N{LATIN SMALL LETTER A WITH ACUTE});
