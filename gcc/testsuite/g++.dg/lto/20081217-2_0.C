struct A {
  virtual int foo() { return 0; }
};
struct B {
  virtual int f() {return 1; }
};
struct C : public A, public B  {
  C();
  virtual int f() { return 0; }
};

C::C()
{
}

int
main()
{
  C c;
  return c.f();
}
