// Koenig lookup is not defined as intended in the std.  DR 218 gives
// an indication of what is meant.  This test case encapsulates the
// current conservative behaviour

// Copyright (C) 2006 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Aug 2006 <nathan@codesourcery.com>

namespace N
{
  struct A {};
  void One (...);  // { dg-error "conflict with" "" }
  void (*Two) (...);  // { dg-error "not a function" "" }
  namespace Three {} // { dg-error "lookup finds|not a function" "" }
}

namespace M
{
  struct B {};
  struct One {};  // { dg-error "lookup finds|not a function" "" }
  void (*Two) (...);  // { dg-error "conflict with" "" }
  void Three (...);  // { dg-error "conflict with" "" }
}

namespace O 
{
  struct C {};
  void Two (...); // { dg-error "conflict with" "" }
}
  
void g (N::A *a, M::B *b, O::C *c)
{
  One (a); // ok
  One (b); // { dg-error "in call to" "" }
  One (a, b); // { dg-error "in call to" "" }

  Two (a); // ok
  Two (a, a); // ok
  Two (b); // ok
  Two (c); // ok
  Two (a, b); // { dg-error "in call to" "" }
  Two (a, c); // { dg-error "in call to" "" }
  
  Three (a); // { dg-error "in call to" "" }
  Three (b); // ok
  Three (a, b); // { dg-error "in call to" "" }
}
