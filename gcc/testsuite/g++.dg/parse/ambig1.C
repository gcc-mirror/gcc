// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 Jan 2003 <nathan@codesourcery.com>

// PR 9109. Ambiguity. [dcl.ambig.res]/7

template <typename T> void Foo (int (T))
{
  try {}
  catch (int (T)) {}
}
