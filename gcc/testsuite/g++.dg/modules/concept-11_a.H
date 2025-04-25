// PR c++/119939
// { dg-additional-options "-fmodule-header -std=c++20" }
// { dg-module-cmi {} }

template <typename T> concept A = true;
template <typename T> concept B = requires { T{}; };
template <typename T> struct S {
  friend bool operator==(const S&, const S&) requires B<T> = default;
};
