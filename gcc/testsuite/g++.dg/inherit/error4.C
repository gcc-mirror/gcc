struct A { virtual ~A(); };

struct B : A A {};            // { dg-error "'A'|function|extra" }

A foo(const B &b)
{
  return b;                   // { dg-error "conversion" }
}
