// DR 1710 - Missing template keyword in class-or-decltype
// { dg-do compile }

template<typename T> struct A {
  template<typename U> struct B {
  };
};

template<typename T> struct D : A<int>::B<int> {};
template<typename T> struct D2 : A<int>::template B<int> {};
