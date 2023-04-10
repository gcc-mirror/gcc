/* { dg-do compile } */
/* { dg-options "-O1 -ftree-loop-vectorize -fno-tree-dce -fno-tree-scev-cprop" } */

int m;

__attribute__ ((noinline, returns_twice)) void
empty (void)
{
}

void
foo (void)
{
  while (m < 1)
    {
      empty ();
      ++m;
    }
}
