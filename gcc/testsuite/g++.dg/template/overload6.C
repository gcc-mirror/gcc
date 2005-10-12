// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 12 Oct 2005 <nathan@codesourcery.com>

// PR 21592:ICE
// Origin:  Volker Reichelt <reichelt@gcc.gnu.org>

template<typename T> void unique(T,T);

struct A
{
  int begin();
};

template<int> void foo()
{
  unique(A().begin); // { dg-error "no matching function" "" }
}
