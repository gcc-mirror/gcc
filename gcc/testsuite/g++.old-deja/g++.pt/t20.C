// { dg-do assemble  }

template <class X> class A {
public:
  X aaa;
  int foo();
};

template <class X> A<X> f(X);

void frep() {
  int x;
  x = f(6.4).foo();
}

