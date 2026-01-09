// PR c++/122609
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fmodules" }
// { dg-module-cmi U }

export module U;
import K;

namespace std {
  template <typename T> struct tuple_size { static constexpr int value = 1; };
  template <int I, typename T> struct tuple_element { using type = int; };
};

export struct S {};
template <int I> int get(S) { return 123; };

export template <typename T> void call_use_without_import(T t) {
  use_without_import(t);
}
