// PR c++/84874
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A { int a, b; };
struct B { A d; };

void
foo (B *x)
{
  *x = { .d = { .b = 5 } };	// { dg-message "non-trivial designated initializers not supported" }
}

void
bar (A *x)
{
  *x = { .b = 6 };		// { dg-message "non-trivial designated initializers not supported" }
}
