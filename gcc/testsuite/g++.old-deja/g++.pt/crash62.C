// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 Jan 2001 <nathan@codesourcery.com>

// Bug 911, ICE on bogus template declaration

template <class T> class A<T>;    // ERROR - not a template
