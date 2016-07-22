// Koenig lookup is not defined as intended in the std.  DR 218 gives
// an indication of what is meant.  This test case encapsulates the
// current conservative behavior

// Copyright (C) 2006 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Aug 2006 <nathan@codesourcery.com>

namespace N
{
  struct A {};
  void One (...);		// { dg-message "N::One" }
  void (*Two) (...);		// { dg-message "N::Two" }
  namespace Three {}		// { dg-message "N::Three" }
}

namespace M
{
  struct B {};
  struct One {};		// { dg-message "M::One" }
  void (*Two) (...);		// { dg-message "M::Two" }
  void Three (...);		// { dg-message "M::Three" }
}

namespace O 
{
  struct C {};
  void Two (...);		// { dg-message "O::Two" }
}
  
void g (N::A *a, M::B *b, O::C *c)
{
  One (a); // ok
  One (a, b); // ok
  One (b); // { dg-error "3:'One' was not declared" }
  // { dg-message "suggested alternatives" "suggested alternative for One" { target *-*-* } 34 }

  Two (c); // ok
  Two (a, c); // ok
  Two (a); // { dg-error "3:'Two' was not declared" }
  // { dg-message "suggested alternatives" "suggested alternative for Two" { target *-*-* } 39 }
  Two (a, a); // error masked by earlier error
  Two (b); // error masked by earlier error
  Two (a, b); // error masked by earlier error
  
  Three (b); // ok
  Three (a, b); // ok
  Three (a); // { dg-error "3:'Three' was not declared" }
  // { dg-message "suggested alternatives" "suggested alternative for Three" { target *-*-* } 47 }
}
