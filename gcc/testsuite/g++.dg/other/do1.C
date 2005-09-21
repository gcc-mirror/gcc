// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 16 Sep 2002 <nathan@codesourcery.com>

// PR 7640. ICE.

void init ()
{
  do {  } while (0)
	    obj = 0; // { dg-error "expected|not declared" "" }
     
}
