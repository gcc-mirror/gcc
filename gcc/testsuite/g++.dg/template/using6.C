namespace foo {
  template<typename T>
  struct A {};
}

namespace bar {
  template<typename T>
  struct A {};
}

namespace foo {
  using bar::A; // { dg-error "" }
}

