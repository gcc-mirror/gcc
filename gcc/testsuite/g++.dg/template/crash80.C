// PR c++/37087

namespace a {
  template <typename T> class Foo;
}

namespace b {
  template <> class ::a::Foo<double> {}; // { dg-error "error: global qualification of class name is invalid" }
}
