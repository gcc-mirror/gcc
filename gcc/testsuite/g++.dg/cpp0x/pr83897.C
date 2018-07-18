// PR c++/83897
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fdump-tree-gimple" }
// { dg-final { scan-tree-dump-not "__builtin_unreachable" "gimple" } }

struct A {};
struct B { int a; int b = 5; };

A
bar (B)
{
  return {};
}
