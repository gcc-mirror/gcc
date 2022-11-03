// PR c++/66985
// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template <template <class> class T>
concept bool Valid = requires { typename T<int>; };

template <template <class> class T>
struct __defer { };

Valid{T}
struct __defer<T> {
  using type = T<int>;
};
