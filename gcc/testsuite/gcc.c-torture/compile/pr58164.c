/* PR tree-optimization/58164 */
/* { dg-require-effective-target indirect_jumps } */

int
foo (void)
{
  int x = 0;
  goto *&x;
}
