/* P2290R3 - Delimited escape sequences */
/* { dg-do compile } */
/* { dg-require-effective-target wchar } */
/* { dg-options "-std=gnu99 -Wno-c++-compat -Wno-unicode" { target c } } */
/* { dg-options "-std=gnu++20 -Wno-unicode" { target c++ } } */

#define z(x) 0
#define a z(
int b = a\u{});		/* { dg-bogus "empty delimited escape sequence; treating it as separate tokens" } */
int c = a\u{);		/* { dg-bogus "'\\\\u\\\{' not terminated with '\\\}' after \\\\u\\\{; treating it as separate tokens" } */
int d = a\u{12XYZ});	/* { dg-bogus "'\\\\u\\\{' not terminated with '\\\}' after \\\\u\\\{12; treating it as separate tokens" } */
int e = a\u123);
int f = a\U1234567);
