// PR c++/23694
 
extern "C" struct A
{
  friend void foo(int) {} // { dg-message "declaration" }
  friend void foo() {} // { dg-error "foo" "err" }
};
