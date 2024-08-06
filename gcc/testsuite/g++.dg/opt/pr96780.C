// PR c++/96780
// Verify calls to std::move/forward are folded away by the frontend.
// { dg-do compile { target c++11 } }
// { dg-additional-options "-ffold-simple-inlines -fdump-tree-gimple" }

#include <utility>

struct A;

extern A& a;
extern const A& ca;

void f() {
  auto&& x1 = std::move(a);
  auto&& x2 = std::forward<A>(a);
  auto&& x3 = std::forward<A&>(a);

  auto&& x4 = std::move(ca);
  auto&& x5 = std::forward<const A>(ca);
  auto&& x6 = std::forward<const A&>(ca);

  auto x7 = std::addressof(a);
  auto x8 = std::addressof(ca);
#if __GLIBCXX__
  auto x9 = std::__addressof(a);
  auto x10 = std::__addressof(ca);
#endif
#if __cpp_lib_as_const
  auto&& x11 = std::as_const(a);
  auto&& x12 = std::as_const(ca);
#endif
#if __cpp_lib_forward_like
  auto&& x13 = std::forward_like<int&&>(a);
  auto&& x14 = std::forward_like<int&&>(ca);
#endif
}

// { dg-final { scan-tree-dump-not "= std::move" "gimple" } }
// { dg-final { scan-tree-dump-not "= std::forward" "gimple" } }
// { dg-final { scan-tree-dump-not "= std::addressof" "gimple" } }
// { dg-final { scan-tree-dump-not "= std::__addressof" "gimple" } }
// { dg-final { scan-tree-dump-not "= std::as_const" "gimple" } }
// { dg-final { scan-tree-dump-not "= std::forward_like" "gimple" } }
