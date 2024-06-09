// PR c++/104919
// PR c++/106009
// { dg-additional-options -fmodules-ts }
// { dg-module-cmi Enum13 }

export module Enum13;

export
constexpr int f() {
  enum E { e = 42 };
  return e;
}

template<class T>
constexpr int ft(T) {
  enum E { e = 43 };
  return e;
}

export
constexpr int g() {
  return ft(0);
}
