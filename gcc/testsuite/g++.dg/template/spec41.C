// PR c++/94255 - crash with template spec in different namespace.
// { dg-do compile { target c++11 } }

namespace N {
  class S {
    template <typename> struct foo;
  };
  namespace M {
    using S = ::N::S;
  }
}

namespace N {
  namespace M {
    template <> struct S::foo<int> {}; // { dg-error "specialization of" }
  }
}
