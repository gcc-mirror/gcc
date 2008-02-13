// PR c++/34824

struct A;

struct B
{
  B (A const &);		// { dg-warning "note" }
  B (B &);			// { dg-warning "note" }
};

struct A
{
  A (B);
};

B
f (B const& b)
{
  return b;			// { dg-error "" }
}
