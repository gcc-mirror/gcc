// PR c++/86094
// { dg-do compile { target c++11 } }
// { dg-additional-options "-Wabi=11 -fdump-tree-gimple" }
// { dg-final { scan-tree-dump-not "struct S &" "gimple" } }

struct S {
  S(S&&) = default;
  int i;
};

S foo(S s)
{
  return s;
}
