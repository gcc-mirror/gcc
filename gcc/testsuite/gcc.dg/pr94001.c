/* PR tree-optimization/94001 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dce" } */

void
bar (int e)
{
  bar (3);
  int c;
  c = -e;
}
