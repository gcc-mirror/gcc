// { dg-do assemble  }
// Test that the result of `x ? const E : E' is an E rvalue.
// Contributed by Jason Merrill <jason@cygnus.com>

enum E { a };

bool b;

int main ()
{
  E e1 = a;
  const E &er = e1;
  E e2 = b ? er : a;		// OK
  const E* ep = &(b ? er : a);	// { dg-error "" } non-lvalue
}
