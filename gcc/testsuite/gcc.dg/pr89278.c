/* PR tree-optimization/89278 */
/* { dg-do compile } */
/* { dg-options "-O1 -ftrapv -ftree-loop-distribute-patterns --param max-loop-header-insns=2" } */

void
foo (int *w, int x, int y, int z)
{
  while (x < y + z)
    {
      w[x] = 0;
      ++x;
    }
}

void
bar (int *__restrict u, int *__restrict w, int x, int y, int z)
{
  while (x < y + z)
    {
      w[x] = u[x];
      ++x;
    }
}
