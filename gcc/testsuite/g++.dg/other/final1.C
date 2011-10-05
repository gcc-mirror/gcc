/* Verify that final methods are devirtualized */
/* { dg-do compile } */
/* { dg-options "-fdump-tree-original -std=c++0x"  } */

struct A final
{
  virtual void foo ()
  {
  }
};

struct B
{
  virtual void foo () final
  {
  }
};

void fun(A* a, B* b)
{
  a->foo();
  b->foo();
}

/* { dg-final { scan-tree-dump-times "A::foo" 2 "original"  } } */
/* { dg-final { scan-tree-dump-times "B::foo" 2 "original"  } } */
/* { dg-final { cleanup-tree-dump "original" } } */
