// Warn when a declaration is specified with greater visibility than that
// of its type.

// { dg-do compile }
// { dg-require-visibility "" }
// { dg-final { scan-hidden "_Z1fv" } }

namespace N __attribute ((__visibility__ ("hidden")))
{
  struct A { };
}

N::A f() { } // { dg-warning "visibility" "" }
