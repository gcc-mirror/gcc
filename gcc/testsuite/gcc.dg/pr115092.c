/* PR rtl-optimization/115092 */
/* { dg-do run } */
/* { dg-options "-O1 -fgcse -ftree-pre -fno-tree-dominator-opts -fno-tree-fre -fno-guess-branch-probability" } */

int a, b, c = 1, d, e;

int
main ()
{
  int f, g = a;
  b = -2;
  f = -(1 >> ((c && b) & ~a));
  if (f <= b)
    d = g / e;
  return 0;
}
