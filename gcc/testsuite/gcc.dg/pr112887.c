/* PR tree-optimization/112887 */
/* { dg-do compile } */
/* { dg-options "-O2 --param=l1-cache-line-size=0x20000000" } */

void bar (long);
long c;
struct S { long a, b; } s;

void
foo (void)
{
  bar (c ? s.a : s.b);
}
