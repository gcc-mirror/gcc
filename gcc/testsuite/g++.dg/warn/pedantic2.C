// { dg-options "-pedantic" }

class foo
{
  foo() {};
  void bar() {};

  foo(int) {};;  // { dg-warning "extra" "" { target c++98_only } }
  void bar(int) {};;  // { dg-warning "extra" "" { target c++98_only } }
};
