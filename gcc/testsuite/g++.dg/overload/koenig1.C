// PR c++/9336
// Bug: The first, invalid call to f caused us to crash on the second,
// valid call.

namespace N {
  template <class T> void f (T); // { dg-message "N::f" }
  struct A;
}

struct B;

void g ()
{
  B *bp;
  N::A *ap;
  f (bp);			// { dg-error "3:'f' was not declared" }
  // { dg-message "suggested alternative" "suggested alternative" { target *-*-* } 16 }
  f (ap);
}
