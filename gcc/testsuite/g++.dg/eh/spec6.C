// Test that we don't allow incomplete types in an exception-specification
// for a definition, or at a call site.

// { dg-options "-fpermissive -w" }

struct A;			// { dg-message "" }

struct B
{
  void f () throw (A);
};

void B::f () throw (A) {}	// { dg-error "A" }

int main ()
{
  B b;
  b.f();			// { dg-error "A" }
}
