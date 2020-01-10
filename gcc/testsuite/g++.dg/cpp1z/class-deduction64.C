// PR c++/88820
// { dg-do compile { target c++17 } }

template <int> struct S;

template <S> struct W {	  // { dg-error "class type" "" { target c++17_only } }
  template <typename> static int foo();
  bool b = foo<int>();
};
