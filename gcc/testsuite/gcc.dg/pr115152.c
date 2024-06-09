/* PR tree-optimization/115152 */
/* { dg-do run } */
/* { dg-options "-O3 -fno-tree-fre -fno-tree-dominator-opts -fno-tree-loop-im" } */

int a, b, c, d;
signed char e[1] = { 1 };

int
main ()
{
  for (a = 0; a < 3; a++)
    for (b = 0; b < 2; b++)
      c = e[0] = e[0] ^ d;
  if (!c)
    __builtin_abort ();
  return 0;
}
