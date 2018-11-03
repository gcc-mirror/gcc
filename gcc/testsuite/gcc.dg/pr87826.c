/* PR tree-optimization/87826 */
/* { dg-do compile } */
/* { dg-options "-O3 -w" } */

int c;

void
foo (int *b)
{
  int e;
  for (e = 0; e < 16; ++e)
    b[e] = c >> e * 8;
}
