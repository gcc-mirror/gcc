/* { dg-do compile } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-mcpu=nps400 -O2 -mbitops" } */

struct { unsigned a: 5, b: 8, c: 19; } foo;
struct { unsigned a: 3, b: 8, c: 21; } bar;

void
f (void)
{
  bar.b = foo.b;
}
/* { dg-final { scan-assembler "movb\[ \t\]+r\[0-5\]+, *r\[0-5\]+, *r\[0-5\]+, *5, *3, *8" { target arceb-*-* } } } */
/* { dg-final { scan-assembler "movb\[ \t\]+r\[0-5\]+, *r\[0-5\]+, *r\[0-5\]+, *3, *5, *8" { target arc-*-* } } } */
