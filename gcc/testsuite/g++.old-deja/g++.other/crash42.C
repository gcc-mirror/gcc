// { dg-do assemble  }
// { dg-options "-g" }
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 25 Jul 2001 <nathan@codesourcery.com>

// Bug 3152. Using a typedef to declare a function used an unset
// global variable, last_function_parms.

struct actor 
{
  typedef bool (operation)();

  operation a;
  operation b;
  operation c;
};
