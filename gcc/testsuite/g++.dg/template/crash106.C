// PR c++/47974

typedef double T;

struct A
{
  template<T> void foo(); // { dg-error "type" "" { target c++17_down } }
};

template<T N = 0.0, void (A::*)() = &A::foo<N> > struct B {}; // { dg-error "type|declared|could not convert" "" { target c++17_down } }

B<> b; // { dg-message "" "" { target c++17_down } }
