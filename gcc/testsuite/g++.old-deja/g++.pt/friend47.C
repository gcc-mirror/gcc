// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 Jan 2001 <nathan@codesourcery.com>

// Bug 1033. We ICE'd when trying to make a non template class a templated
// friend.

class A {};
class B {
  template<class T> friend class A;   // ERROR - not a template
};


