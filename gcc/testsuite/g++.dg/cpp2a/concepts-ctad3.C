// PR c++/97679
// { dg-do compile { target c++20 } }

template <bool V> struct A {
  template <class T> struct B {
    B(T) requires V;
    template <class U> B(T, U) requires V || (__is_same(T, char) && __is_same(U, int));
  };
};

A<true>::B x1(0);
A<false>::B x2(0); // { dg-error "deduction|no match" }

A<true>::B y1(0, '0');
A<false>::B y2(0, '0'); // { dg-error "deduction|no match" }
A<false>::B y3('0', 0);
