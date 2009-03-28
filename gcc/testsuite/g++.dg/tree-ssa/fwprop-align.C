/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop2" } */

struct A
{
  void foo ()
  {
  }
};

int main()
{
  void (A::* const p)() = & A::foo;
  A a;
  (a.*p)();
}

/* We should eliminate the check if p points to a virtual function. */
/* { dg-final { scan-tree-dump-times "& 1" 0 "forwprop2" } } */
/* { dg-final { cleanup-tree-dump "forwprop2" } } */
