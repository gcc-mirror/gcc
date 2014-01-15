// PR c++/59659
// { dg-options "-fdump-tree-gimple" }
// { dg-final { scan-tree-dump-times "S::S" 1 "gimple" } }
// { dg-final { cleanup-tree-dump "gimple" } }

struct S { S (); S (int i); int i; };
struct A { S s[100]; };

void
foo ()
{
  A a = {{}};
}
