// PR c++/122609
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fmodules" }
// { dg-module-cmi K }

export module K;
export template <typename T> void use_without_import(T t) {
  auto [x] = t;
}
