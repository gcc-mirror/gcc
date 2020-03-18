// PR c++/94057 - template keyword in a typename-specifier.
// { dg-do compile { target c++11 } }

template<typename T> struct A {
  template<typename U> struct B {
    typedef int TT;
    typedef int TT2;
    typedef int TT3;
    typedef int TT4;
  };
};

struct X : A<int>::B<int> {
  using A<int>::template B<int>::TT;
  using typename A<int>::template B<int>::TT2;
  using A<int>::B<int>::TT3;
  using typename A<int>::B<int>::TT4;
};
