// { dg-options "-fpermissive -w" }

namespace N {
  template <typename T>
  struct S {
    void f() {}  // { dg-bogus "from definition" }
  };
}

namespace K {
  template <> void N::S<char>::f() {}
}
