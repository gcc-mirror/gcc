// Test whether -Wunused handles empty classes the same as non-empty.
// { dg-do compile }
// { dg-options "-Wunused" }

struct A {};
struct B { char c; };

void foo ()
{
  struct A a0, a1;
  struct B b0, b1 = { 25 };

  a0 = a1;	// { dg-bogus "value computed is not used" }
  b0 = b1;
}
