// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 8 Mar 2005 <nathan@codesourcery.com>

// PR 20186: ICE
// Origin: Jan Dvorak <jan.dvorak@kraxnet.cz>

template<typename T> void foo(T &t)
{
  int i = static_cast<int>(t);
}
