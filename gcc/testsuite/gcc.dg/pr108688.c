/* PR tree-optimization/108688 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-pre -fno-tree-fre -fno-tree-dominator-opts -fno-tree-loop-im -fno-code-hoisting" } */

union U { signed int d : 7; signed int e : 2; } u;
int a, b;

void
foo (void)
{
  for (int i = 0; i < 64; i++)
    {
      u.d = a;
      u.d ^= b;
    }
}
