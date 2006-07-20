// Tests for various visibility mismatch situations.

// { dg-require-visibility "" }

// { dg-final { scan-not-hidden "_ZN1A1fEv" } }

struct __attribute ((visibility ("hidden"))) A
{
  // This is OK, A::f gets default visibility.
  __attribute ((visibility ("default"))) void f ();
};

void A::f() { }

// This gets a warning; it should have explicit visibility of some sort.
A* afactory1() { return new A; }	// { dg-warning "visibility" }

// This is OK.
__attribute ((visibility ("default"))) A*
afactory2 () { return new A; }

// This gets a warning.
struct B
{				// { dg-warning "visibility" }
  A a;
};

// This one has explicit visibility, so it doesn't get a warning.
struct __attribute ((visibility ("default"))) C
{
  A a;
};
