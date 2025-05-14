/* PR rtl-optimization/119594 */
/* { dg-do run } */
/* { dg-options "-Os -fno-dce -fno-tree-dce -fno-tree-dse" } */

int a, b;
static unsigned c = -1;

void
foo (int e)
{
  a = a ^ e;
}

void
bar (long e)
{
  foo (e >> 1);
}

int
main ()
{
  int g[2];
  for (int h = 0; h < 2; h++)
    g[h] = -1;
  for (; b; b++)
    ;
  g[1] = 0;
  bar (c);
  if (!a)
    __builtin_abort ();
}
