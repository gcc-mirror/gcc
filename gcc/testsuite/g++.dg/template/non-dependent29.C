// PR c++/112427

struct A { int m; void f(); };
struct B { A a; };

template<class T>
void f(B b) {
  int A::*pd = &A::m;
  b.a.*pd;

  void (A::*pf)() = &A::f;
  (b.a.*pf)();
}
