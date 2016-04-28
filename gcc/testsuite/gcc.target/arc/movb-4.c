/* { dg-do compile } */
/* { dg-options "-mcpu=nps400 -O2 -mbitops" } */

struct { int a: 13, b: 19; } foo;
struct { int a: 13, b: 19; } bar;

void
f (void)
{
  bar.b = foo.b;
}
/* { dg-final { scan-assembler "movb\[ \t\]+r\[0-5\]+, *r\[0-5\]+, *r\[0-5\]+, *13, *13, *19" { target arc-*-* } } } */
/* { dg-final { scan-assembler "movb\[ \t\]+r\[0-5\]+, *r\[0-5\]+, *r\[0-5\]+, *0, *0, *19" { target arceb-*-* } } } */
