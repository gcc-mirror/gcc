// { dg-options "-pedantic" }

class foo
{
  foo() {};
  void bar() {};

  foo(int) {};;  // { dg-warning "extra" }
  void bar(int) {};;  // { dg-warning "extra" }
};
