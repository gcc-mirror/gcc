// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M:B }

export module M:B;
import :A;

export template <typename U> struct B {
  int foo(A<U> a) { return a.x; }
};
