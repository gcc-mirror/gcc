// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Jun 2004 <nathan@codesourcery.com>

// Origin: Volker Reichelt <reichelt@gcc.gnu.org>
// Bug 16260. ICE

template<typename T> int foo() { return T::X::Y; }
