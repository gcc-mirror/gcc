// PR c++/29732

struct A
{
  template<int> template<typename T> friend void foo(T) {} // { dg-error "parameter" }
  void bar() { foo(0); } // { dg-error "foo" }
};
