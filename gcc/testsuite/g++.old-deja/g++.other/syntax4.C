// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 June 2000 <nathan@codesourcery.com>

// Origin GNATS bug report 262 from Jeremy Sanders <jss@ast.cam.ac.uk>
// and several others. With templates, it's very easy to say something
// erroneous like
//    template class X::X<whatever>
// The culprit
//    ... class X::X ...
// caused us to ICE as we got confused about pushing and popping scopes.

class X {
  X ();
};

class Y {
  public:
  typedef ::X W;
  class Z;
};

class Y::Z {};
class Y::W  () {}  // ERROR - parse error
Y::W::X () {}
