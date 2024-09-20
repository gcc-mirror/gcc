// PR c++/116492
// CWG 2789
// { dg-do compile { target c++20 } }

template<class T>
struct A {
  A() requires true = delete;
};

struct B : A<int> {
  B();
  using A<int>::A;
};

B b; // OK, selects the non-inherited constructor over the more constrained
     // inherited constructor.
