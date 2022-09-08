/* P2290R3 - Delimited escape sequences */
/* { dg-do compile } */
/* { dg-require-effective-target wchar } */
/* { dg-options "-std=gnu99 -Wno-c++-compat" { target c } } */
/* { dg-options "-std=gnu++20" { target c++ } } */

#define z(x) 0
#define a z(
int b = a\u{});		/* { dg-warning "empty delimited escape sequence; treating it as separate tokens" } */
int c = a\u{);		/* { dg-warning "'\\\\u\\\{' not terminated with '\\\}' after \\\\u\\\{; treating it as separate tokens" } */
int d = a\u{12XYZ});	/* { dg-warning "'\\\\u\\\{' not terminated with '\\\}' after \\\\u\\\{12; treating it as separate tokens" } */
int e = a\u123);
int f = a\U1234567);
