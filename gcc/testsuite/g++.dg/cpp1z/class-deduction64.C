// PR c++/88820
// { dg-do compile { target c++17 } }

template <int> struct S;

template <S> struct W {
  template <typename> static int foo();
  bool b = foo<int>();
};
