// { dg-do run  }
// prms-id: 3041

class A {
public:
  A() { }
  virtual void a() = 0;
  static int b(A * p) {
    p->a();
    return 1;
  }
};

class B : virtual public A {
public:
  B() {
    static int installed = b(this);
  }
  void a() { }
};

class C : virtual public B {
public:
  C() {
    static int installed = b(this);
  }
  void a() { }
};

int main()
{
  C c;
  return 0;
}
