// { dg-do assemble  }

struct B { int foo (); };
int B::foo() { return 37; }

template <class A> struct X {
  void f(int);
};

template <class A> void X<A>::f (int jj)
{}

X<int> x;

void xxx () { x.f (1); }
