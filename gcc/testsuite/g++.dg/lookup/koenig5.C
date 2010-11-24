// Koenig lookup is not defined as intended in the std.  DR 218 gives
// an indication of what is meant.  This test case encapsulates the
// current conservative behaviour

// Copyright (C) 2006 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Aug 2006 <nathan@codesourcery.com>

namespace N
{
  struct A {};
  void One (...);
  void (*Two) (...);
  namespace Three {}
}

namespace M
{
  struct B {};
  struct One {};
  void (*Two) (...);
  void Three (...);
}

namespace O 
{
  struct C {};
  void Two (...);
}
  
void g (N::A *a, M::B *b, O::C *c)
{
  One (a); // ok
  One (a, b); // ok
  One (b); // { dg-error "not declared" }

  Two (c); // ok
  Two (a, c); // ok
  Two (a); // { dg-error "not declared" }
  Two (a, a); // error masked by earlier error
  Two (b); // error masked by earlier error
  Two (a, b); // error masked by earlier error
  
  Three (b); // ok
  Three (a, b); // ok
  Three (a); // { dg-error "not declared" }
}
