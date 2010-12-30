// PR c++/34824

struct A;

struct B
{
  B (A const &);		// { dg-message "note" }
  B (B &);			// { dg-message "note" }
};

struct A
{
  A (B);			// { dg-error "initializing" }
};

B
f (B const& b)
{
  return b;			// { dg-error "matching" "matching" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 19 }
}
