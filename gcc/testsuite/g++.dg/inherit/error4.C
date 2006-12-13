//PR c++/28740

struct A { virtual ~A(); };

struct B : A A {};            // { dg-error "'A'|function definition|extra" }

A foo(const B &b)
{
  return b;                   // { dg-error "conversion" }
}
