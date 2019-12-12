// PR c++/79899

// { dg-do compile }
// { dg-options "-Os" }

struct A
{
  friend A::~A() {} // { dg-error "3:member functions are implicitly friends of their class" }
};
