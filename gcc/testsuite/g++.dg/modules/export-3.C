// P2615R1 invalid declarations
// PR c++/107688
// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi !bad }

export module bad;

extern "C++" export void foo();  // { dg-error "export-declarations are not permitted here" "" { target c++20 } }

export template <typename T> struct S {};

export template <typename T> struct S<T*> {};  // { dg-error "partial specialization in unbraced export-declaration" }

export template <> struct S<int*> {};  // { dg-error "explicit specializations are not permitted here" }

export template struct S<int>;  // { dg-error "explicit instantiations are not permitted here" }

template <> export struct S<double>;  // { dg-error "expected unqualified-id" }

export export int x;  // { dg-error ".export. may only occur once" }

export { export int y; }  // { dg-error ".export. may only occur once" }

namespace {
  export namespace ns {}  // { dg-error "internal linkage" }
}

export namespace {}  // { dg-error "exporting unnamed namespace" }
