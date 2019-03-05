// PR c++/66985
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template <template <class> class T>
concept bool _Valid = requires { typename T<int>; };

template <template <class> class T>
struct __defer { };

_Valid{T}
struct __defer<T> {
  using type = T<int>;
};
