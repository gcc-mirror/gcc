// prms-id: 3139

extern "C" int printf(const char *, ...);

class A {
  public:
  A() { }
  virtual int a() = 0;
};

class B : virtual public A {
  public:
  virtual int a() = 0;
};

class C : public B {
  public:
  int a() { return 42; }
};

int main() {
  B * b = new C;
  printf("%d.\n", b->a());
  return 0;
}
