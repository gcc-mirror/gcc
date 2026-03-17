// PR c++/124200
// { dg-do compile { target c++26 } }
// { dg-additional-options "-fmodules -freflection -Wno-global-module" }
// { dg-module-cmi M }

module;
namespace ns {
  inline void b() {}
  inline void c() {}
}
export module M;
namespace ns {
  export using ns::b;
  export extern "C++" inline void d() {}
  void e() {}
  export void f() {}

  export struct S {} S;
  export struct T {};
  T T;
  struct U {};
  export U U;
}
