// PR c++/59082

struct A {};

struct B : virtual A, A {};  // { dg-error "duplicate base type" }

A foo(const B &b)
{
  return b;
}
