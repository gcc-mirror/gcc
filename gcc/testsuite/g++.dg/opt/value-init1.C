// PR c++/59659
// { dg-options "-fdump-tree-gimple -std=c++11" }
// { dg-final { scan-tree-dump-times "i = 0" 0 "gimple" } }
// { dg-final { cleanup-tree-dump "gimple" } }

struct S { S () = default; S (int i); int i; };
struct A { S s[100]; };

void
foo ()
{
  A a = {{}};
}
