// Test that using an elaborated-type-specifier in a namespace to refer
// to a class outside the namespace does not cause its name to be considered
// declared in the namespace.

// Contributed by Jason Merrill <jason@cygnus.com>
// Build don't link:

struct A { };

int A;

namespace N {
  struct A *f ();
}

using namespace N;

struct A *a;
