// GROUPS passed vtable
extern "C" int printf (const char *, ...);
extern "C" void exit(int);

class A {
public:
  virtual const char* f1() { return "A::f1"; }
  virtual const char* f2() { return "A::f2"; }
  virtual const char* f3() { printf("FAIL\n"); exit(1); return "A::f3"; }
};

class B {
public:
  virtual const char* f2() { return "B::f2"; }
  virtual const char* f3() { return "B::f3"; }
};

class C: public A, public B {
public:
  const char* f2() { return B::f2(); }
  const char* f1() { return f2(); }
  const char* f3() { return A::f3(); }
};

class D: public A, public B {
public:
  const char* f2() { return B::f2(); }
  const char* f1() { return D :: f2(); }
  const char* f3() { return A::f3(); }
};

int main() {
  C* tempC = new C;
  D* tempD = new D;
  A* a = tempC;
  printf("calling f1 on a C gives %s\n", a->f1());
  a = tempD;
  printf("calling f1 on a D gives %s\n", a->f1());
  printf("PASS\n");
  return 0;
}
