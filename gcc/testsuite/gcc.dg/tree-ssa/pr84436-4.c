/* PR tree-optimization/84436 */
/* { dg-options "-O2 -fdump-tree-switchconv -fdump-tree-optimized" } */
/* { dg-do run } */

enum E
{
  A, B, C,
};

int
__attribute__ ((noipa))
foo(enum E e)
{
  switch (e)
    {
    case A: return 0;
    case B: return 1;
    case C: return 2;
    }

  return -1;
}

int main()
{
  if (foo (A) != 0)
  __builtin_abort ();

  if (foo (B) != 1)
  __builtin_abort ();

  if (foo (C) != 2)
  __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-not "switch" "optimized" } } */
