// { dg-do run  }
class A {
  char a;
public:
  A(char x) : a(x) { }
  virtual ~A() { }
};

class B : virtual public A {
  char b;
public:
  B(char x) : A('b'), b(x) { }
  ~B() { }
};

class C : virtual public A {
  char c;
public:
  C(char x) : A('c'), c(x) { }
  ~C() { }
};

class D : virtual public A, public B, public C {
  char d;
public:
  D(char x) : A('d'), B('d'), C('d'), d(x) { }
  ~D() { }
};

class E : virtual public A, public B, public C {
  char e;
public:
  E(char x) : A('e'), B('e'), C('e'), e(x) { }
  ~E() { }
};

class F : virtual public A, public D, public E {
  char f;
public:
  F(char x) : A('f'), D('f'), E('f'), f(x) { }
  ~F() { }
};

int main() {
  F f('x');
  return 0;
}
