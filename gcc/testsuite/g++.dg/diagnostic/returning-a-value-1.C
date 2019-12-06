int foo();

struct A
{
  A() { return foo(); }  // { dg-error "19:returning a value" }
  ~A() { return foo(); }  // { dg-error "20:returning a value" }
  void bar() { return foo(); }  // { dg-error "26:return-statement with a value" }
};
