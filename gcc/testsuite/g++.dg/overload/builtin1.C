// Test that we don't discard builtin candidates inappropriately.

struct B { };

struct A {
  operator int ();
  operator B ();
};

void operator+ (B, B);		// { dg-error "" "candidate" }

int main ()
{
  A a;
  a + a;			// { dg-error "" "ambiguous" }
}
