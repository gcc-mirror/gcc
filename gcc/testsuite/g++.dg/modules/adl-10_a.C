// PR c++/121893
// { dg-additional-options "-fmodules -Wno-global-module" }
// { dg-module-cmi M }

module;
namespace ns {
  struct S {};

  struct F {
    template <typename T> void operator()(T) {}
  };
  inline constexpr F foo{};
}

export module M;

namespace ns {
  export using ns::S;
  export using ns::foo;
}

template <typename T> void foo(T) {}
export template <typename T> void go(T t) { foo(t); }
