// Test that we don't discard builtin candidates inappropriately.

struct B { };

struct A {
  operator int ();
  operator B ();
};

void operator+ (B, B);		// { dg-message "operator" "operator" }

int main ()
{
  A a;
  a + a;			// { dg-error "ambiguous" "ambiguous" }
  // { dg-message "operator" "match candidate text" { target *-*-* } 15 }
  // { dg-message "candidates" "candidates" { target *-*-* } 15 }
}
