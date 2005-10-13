// PR c++/23694
 
extern "C" struct A
{
  friend void foo(int) {} // { dg-error "declaration" }
  friend void foo() {} // { dg-error "foo" }
};
