// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 Jan 2003 <nathan@codesourcery.com>

// We erroneously prohibited default args on parenthesized function
// declarations.

void (foo)(int i = 0);
