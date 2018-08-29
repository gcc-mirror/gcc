// PR c++/84707
// { dg-do compile { target c++11 } }

inline namespace {
  namespace {}
}

namespace {} // { dg-error "conflicts" }
