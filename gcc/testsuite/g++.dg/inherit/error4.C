//PR c++/28740

struct A { virtual ~A(); };

struct B : A A {};		// { dg-error "expected|initializer|invalid" }

A foo(const B &b)		// { dg-error "" }
{
  return b;
}
