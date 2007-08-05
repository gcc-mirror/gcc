// { dg-options "-pedantic" }

class foo
{
  foo() {};
  void bar() {};

  foo(int) {};;  // { dg-error "extra" }
  void bar(int) {};;  // { dg-error "extra" }
};
