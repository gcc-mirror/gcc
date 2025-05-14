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
  export int y = 456;  // { dg-error "exporting" }
  export void h();  // { dg-error "exporting" }
  export void i() {}  // { dg-error "exporting" }
  export template <typename T> void v(); // { dg-error "exporting" }
  export template <typename T> void w() {} // { dg-error "exporting" }
  export auto [e] = S{};  // { dg-error "exporting" }

  export namespace ns {}  // { dg-error "exporting" }
  export namespace alias = global;  // { dg-error "exporting" }

  export struct A {};  // { dg-error "exporting" }
  export template <typename T> struct B {};  // { dg-error "exporting" }

  export enum E {};  // { dg-error "exporting" }
  export enum class F {};  // { dg-error "exporting" }

  export template <typename T> using U = int;  // { dg-error "exporting" }

#if __cplusplus >= 202002L
  export template <typename T> concept C = true;  // { dg-error "exporting" "" { target c++20 } }
#endif

  // Also complain about exporting no-linkage decls in an unnamed namespace
  export typedef int T;  // { dg-error "exporting" }
  export typedef struct {} *PC;  // { dg-error "exporting" }
  export using V = int;  // { dg-error "exporting" }
}

export namespace {}  // { dg-error "exporting unnamed namespace" }
