struct A {
  virtual int foo() {}
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

main()
{
  C c;
  return c.f();
}
