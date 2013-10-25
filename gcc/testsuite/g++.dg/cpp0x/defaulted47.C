// PR c++/54812
// { dg-do compile { target c++11 } }

class A
{
  A() = default;   // { dg-error "private" }
};

A a;               // { dg-error "context" }

class B
{
  ~B() = default;  // { dg-error "private" }
};

B b;               // { dg-error "context" }
