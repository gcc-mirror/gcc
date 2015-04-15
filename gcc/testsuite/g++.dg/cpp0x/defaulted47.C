// PR c++/54812
// { dg-do compile { target c++11 } }

class A
{
  A() = default;   // { dg-message "private" }
};

A a;               // { dg-error "context" }

class B
{
  ~B() = default;  // { dg-message "private" }
};

B b;               // { dg-error "context" }
