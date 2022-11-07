// PR c++/68666
// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

struct A {
  template <class>
  static constexpr bool val = true;
};

template <class T>
concept bool C = A::val<T>;

C{T} struct B {};
