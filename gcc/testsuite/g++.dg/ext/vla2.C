// { dg-do compile }
// { dg-options "" }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 Mar 2003 <nathan@codesourcery.com>

// PR 9708. We unified a VLA size as a constant. Then issued bogus
// errors.

template <unsigned int N>
char* begin(char (&a) [N] );	// { dg-message "note" }

void bar(int i)
{
  char d[i] ;
  
  begin(d);  // { dg-error "no matching function" "" }
  // { dg-message "candidate" "candidate note" { target *-*-* } 17 }
}
