// PR c++/59659
// { dg-do compile { target c++11 } }
// { dg-options "-fdump-tree-gimple" }
// { dg-final { scan-tree-dump-times "i = 0" 0 "gimple" } }

struct S { S () = default; S (int i); int i; };
struct A { S s[100]; };

void
foo ()
{
  A a = {{}};
}
