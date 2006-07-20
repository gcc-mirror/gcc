// Complain when a class is specified with greater visibility than one of
// its members' types or bases, and when a declaration has greater
// visibility than its type.

// { dg-require-visibility "" }

namespace N __attribute ((__visibility__ ("hidden")))
{
  struct A { };
}

struct B
{				// { dg-warning "visibility" }
  N::A a;
};

N::A f () { }			// { dg-warning "visibility" }

struct C: public N::A { };	// { dg-warning "visibility" }
