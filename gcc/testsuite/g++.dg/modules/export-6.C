// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi !bad }

export module bad;
namespace global {}
struct S { int x; };

export static int x = 123;  // { dg-error "internal linkage" }
export static void f();  // { dg-error "internal linkage" }
export static void g() {}  // { dg-error "internal linkage" }
export template <typename T> static void t();  // { dg-error "internal linkage" }
export template <typename T> static void u() {}  // { dg-error "internal linkage" }

#if __cplusplus >= 202002L
export static auto [d] = S{};  // { dg-error "internal linkage" "" { target c++20 } }
#endif

namespace {
  export int y = 456;  // { dg-error "internal linkage" }
  export void h();  // { dg-error "internal linkage" }
  export void i() {}  // { dg-error "internal linkage" }
  export template <typename T> void v(); // { dg-error "internal linkage" }
  export template <typename T> void w() {} // { dg-error "internal linkage" }
  export auto [e] = S{};  // { dg-error "internal linkage" }

  export namespace ns {}  // { dg-error "internal linkage" }
  export namespace alias = global;  // { dg-error "internal linkage" }

  export struct A {};  // { dg-error "internal linkage" }
  export template <typename T> struct B {};  // { dg-error "internal linkage" }

  export enum E {};  // { dg-error "internal linkage" }
  export enum class F {};  // { dg-error "internal linkage" }

  export template <typename T> using U = int;  // { dg-error "internal linkage" }

#if __cplusplus >= 202002L
  export template <typename T> concept C = true;  // { dg-error "internal linkage" "" { target c++20 } }
#endif
}

export namespace {}  // { dg-error "exporting unnamed namespace" }
