// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 9 Dec 2001 <nathan@nathan@codesourcery.com>

// PR 51
// This example snippet is from the ISO C++ standard, sect 7.5 para 4:

extern "C" typedef void FUNC_c();

class C {
  public:
  static FUNC_c* q;
};
