// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 12 Oct 2005 <nathan@codesourcery.com>

// PR 23797:ICE
// Origin:  Volker Reichelt <reichelt@gcc.gnu.org>

struct A { typedef int X; };

int i = typename A::X();
