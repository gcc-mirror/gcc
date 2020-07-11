// PR c++/90572
// { dg-do compile { target c++20 } }

struct X { X(int); };

template<typename T> struct S {
  friend X::X(T::t);
};

struct W { using t = int; };
S<W> s;
