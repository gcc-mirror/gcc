// PR C++/52369
// { dg-do compile { target { ! c++11 } } }

class B
{
  int const v_; // { dg-message "should be initialized" }
};

struct D : B {};

class A
{
  int& ref; // { dg-message "should be initialized" }
};

struct C : A {};

void f()
{
  D d; // { dg-error "uninitialized" }
  new D; // { dg-error "uninitialized" }
  D();
  new D();

  C c; // { dg-error "uninitialized" }
  new C; // { dg-error "uninitialized" }
  C(); // { dg-error "value-initialization" }
  new C(); // { dg-error "value-initialization" }
}
