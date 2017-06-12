// PR c++/79899

// { dg-do compile }
// { dg-options "-Os" }

struct A
{
  friend A::~A() {} // { dg-error "implicitly friends of their class" }
};
