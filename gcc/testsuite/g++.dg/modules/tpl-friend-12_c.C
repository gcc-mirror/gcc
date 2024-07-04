// { dg-additional-options "-fmodules-ts" }
// { dg-module-cmi M:C }

export module M:C;
import :A;

template <typename T> struct B;
export template <typename T, typename U> int bar(B<T> t, U u) {
  return t.foo(u);
}
