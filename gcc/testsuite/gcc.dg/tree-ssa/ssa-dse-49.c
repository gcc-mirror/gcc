/* { dg-do compile } */
/* { dg-options "-O -fno-tree-dce -fdump-tree-dse1-details" } */

struct X { int i; };
void bar ();
void foo (int b)
{
  struct X x;
  x.i = 1;
  if (b)
    {
      bar ();
      __builtin_abort ();
    }
  bar ();
}

/* { dg-final { scan-tree-dump "Deleted dead store: x.i = 1;" "dse1" } } */
