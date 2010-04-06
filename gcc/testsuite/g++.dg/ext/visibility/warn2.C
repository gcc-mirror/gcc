// Complain when a class is specified with greater visibility than one of
// its members' types or bases.

// { dg-require-visibility "" }

namespace N __attribute ((__visibility__ ("hidden")))
{
  struct A { };
}

struct B			// { dg-warning "visibility" }
{
  N::A a;
};

struct C: public N::A { };	// { dg-warning "visibility" }
