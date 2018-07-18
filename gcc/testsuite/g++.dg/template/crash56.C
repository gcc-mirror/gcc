// Origin: Wolfgang Bangerth <bangerth@dealii.org>

// PR c++/28705
// DR 218 is debating whether this is well formed or not.  We've never
// accepted it (because we'd crash), so we continue to reject it, but
// without crashing.

namespace N
{
  struct A { A (A*); };
}

template<typename T> void g (N::A *p)
{
  (void) A (p); // { dg-message "" }
}
