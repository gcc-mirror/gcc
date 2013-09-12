// PR c++/32384
// { dg-do compile }

template<typename S> struct D
{
  typedef int T;
  S foo ();

  D () { foo ().~T(); }		// { dg-error "10:is not of type" }
};

struct Z
{
  Z () {}
  ~Z () {}
};

D<Z> d;
