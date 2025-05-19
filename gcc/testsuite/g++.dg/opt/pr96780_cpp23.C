// PR c++/96780
// Verify calls to std::move/forward are folded away by the frontend.
// { dg-do compile { target c++23 } }
// { dg-additional-options "-ffold-simple-inlines -fdump-tree-gimple" }

#include <utility>

enum class A : char {a};

extern A& x;

void f() {
  auto&& x1 = std::to_underlying(x);
}

// { dg-final { scan-tree-dump-not "= std::to_underlying" "gimple" } }
