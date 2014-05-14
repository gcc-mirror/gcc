// PR c++/34824

struct A;

struct B
{
  B (A const &);
  B (B &);			// { dg-message "note" }
};

struct A
{
  A (B);			// { dg-message "initializing" }
};

B
f (B const& b)
{
  return b;			// { dg-error "" }
}
