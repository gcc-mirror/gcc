// { dg-do compile }

// Origin: sneechy@hotmail.com

// PR c++/8772: Incorrect diagnostics for template template parameter
// mismatch

template <int> struct A {
  template <int> struct B {
    enum { v = 1 };
  };
};

template <template <int> class F> struct C {
  enum { v = F<1>::v || 2 }; 
};

template <int n> struct D {
  enum { v = C<A<n>::B>::v }; // { dg-error "mismatch|class template" }
};
