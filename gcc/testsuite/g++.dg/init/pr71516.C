// PR c++/71516
// { dg-do compile }

struct A;	// { dg-message "forward declaration of" }
struct B
{ 
  static A a;
};
A B::a = A();	// { dg-error "has initializer but incomplete type|invalid use of incomplete type" }
struct A {};
