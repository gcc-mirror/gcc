// Build don't link:
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 11 April 2001 <nathan@codesourcery.com>
// Origin:stephen.webb@cybersafe.com 

// Bug 2125. TYPE_DECLS never had their DECL_CONTEXT set, which
// confused forward references to classes.

typedef void T;
namespace A {
  class C;
  typedef class C C;
  typedef int T;
  class C
  {
    T i;		// got bogus error, found wrong T
  };
}
