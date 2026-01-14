// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

namespace N { void foo (); }

// P2996 doesn't allow this.
namespace [:^^N:] { } // { dg-error "expected" }

// But this is fine.
namespace A = [:^^N:];
using namespace [:^^A:];

void
g ()
{
  foo ();
}
