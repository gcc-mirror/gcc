// { dg-do assemble  }
// Bug: Instantiating A<int> screws with class bindings for B

template <class T> struct A { };
struct B {
  typedef int foo;
  void f ();
};

void B::f () { A<int> a; foo i; }
