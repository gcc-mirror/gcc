// { dg-do compile }
// { dg-options "-fpermissive -std=c++98" }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 Sept 2001 <nathan@codesourcery.com>

// Bug 4206. We were nesting SCOPE_STMTs badly.


struct A
{
  A ();
  ~A ();
};


void Go( )
{
  for (int i = 1;;)	// { dg-warning "using obsolete binding" "" }
    {
      switch (1) {
      default: {}
      }
      A d;
    }
  i;  // { dg-warning "name lookup" "" }
}
