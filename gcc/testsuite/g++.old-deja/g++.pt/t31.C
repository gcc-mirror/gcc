// { dg-do assemble  }

struct B { int foo (); };
int B::foo() { return 37; }

template <class A> struct X {
  void f();
};

template <class A> void X<A>::f ()
{}

X<int> x;

void xyzzy () {
  x.f ();
}
