// { dg-do run  }
// prms-id: 4511

int bad;

class A {
public:
  virtual void dummy (){}
};

class B {
public:
  virtual void f(void) = 0;
};

class C : public A, public B {
public:
  void f(void) { bad=1; }
};

class D : public C {
public:
  void f(void) { }
};

class E : public D { };

int main() {
  E e;
  e.f();
  E * ep = &e;
  ep->f();
  return bad;
}
