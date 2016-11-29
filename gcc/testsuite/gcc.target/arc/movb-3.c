/* { dg-do compile } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-mcpu=nps400 -O2 -mbitops" } */

struct { int a: 23, b: 9; } foo;
struct { int a: 23, b: 9; } bar;

void
f (void)
{
  bar.a = foo.a;
}
/* { dg-final { scan-assembler "movb\[ \t\]+r\[0-5\]+, *r\[0-5\]+, *r\[0-5\]+, *0, *0, *23" { target arc-*-* } } } */
/* { dg-final { scan-assembler "movb\[ \t\]+r\[0-5\]+, *r\[0-5\]+, *r\[0-5\]+, *9, *9, *23" { target arceb-*-* } } } */
