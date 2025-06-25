/* PR middle-end/120630 */
/* { dg-do run } */
/* { dg-options "-O3 -fno-tree-loop-im -fno-tree-loop-optimize -fno-tree-ch" } */

int a, c, d;

void
foo (int b)
{
  a = b;
}

int
main ()
{
  while (d)
    ;
  for (c = 0; c > -3; c--)
    {
      long f = c;
      foo (f >> 2);
    }
  if (a != -1)
    __builtin_abort ();
}
