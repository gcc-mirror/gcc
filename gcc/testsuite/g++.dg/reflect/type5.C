// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Distilled from [temp.res.general].

enum class Enum { A, B, C };
template<class T> struct S {
  using Alias = [:^^int:];
  auto h() -> [:^^S:]<T*>;
  using enum [:^^Enum:];
};
