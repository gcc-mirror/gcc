// PR c++/123578
// { dg-do compile { target c++20 } }
// { dg-options "-fdump-tree-all" }

namespace {
  template <typename>
  struct A { A (decltype ([] { return 0; } ())) {} };
  A <int> b = 0;
}
