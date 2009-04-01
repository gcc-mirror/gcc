// { dg-do assemble  }
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 Feb 2001 <nathan@codesourcery.com>

// Bug 75. using declarations cannot introduce functions which ambiguate
// those in the current namespace, BUT here we're reaccessing the current
// namespace -- the function is not being 'introduced'.

extern int a();
struct x {};

using ::x;
using ::a;

extern "C" void foo ();		// { dg-error "previous declaration" }

namespace {
  extern "C" int foo ();	// { dg-error "C.*linkage" }
  using ::foo; // { dg-error "" } already in use
}
