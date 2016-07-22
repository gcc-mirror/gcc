/* { dg-do compile } */
/* { dg-options "-mcpu=nps400 -O2 -mbitops" } */

struct { unsigned a: 23, b: 9; } foo;
struct { unsigned a: 23, b: 9; } bar;

void
f (void)
{
  bar.b = foo.b;
}
/* { dg-final { scan-assembler "movb\[ \t\]+r\[0-5\]+, *r\[0-5\]+, *r\[0-5\]+, *23, *23, *9" { target arc-*-* } } } */
/* { dg-final { scan-assembler "movb\[ \t\]+r\[0-5\]+, *r\[0-5\]+, *r\[0-5\]+, *0, *0, *9" { target arceb-*-* } } } */
