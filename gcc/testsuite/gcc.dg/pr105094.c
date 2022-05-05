/* PR tree-optimization/105094 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

struct S { short a; char b[~(__SIZE_TYPE__)0 / __CHAR_BIT__ - 1]; };
void bar (struct S *);

void
foo (void)
{
  struct S s = { 5 };
  bar (&s);
}
