// PR c++/103455

struct A { };

struct B {
  operator A*() const;
  template<class T> operator T*() const;
};

typedef void (A::*F)();

void f(B b, F pmf) {
  (b->*pmf)();
}
