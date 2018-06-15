// PR c++/86094
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fabi-version=12 -Wabi -fdump-tree-gimple" }
// { dg-final { scan-tree-dump "struct S &" "gimple" } }

struct S {			// { dg-message "" }
  S(S&&) = default;
  int i;
};

S foo(S s)			// { dg-warning "calling convention" }
{
  return s;
}
