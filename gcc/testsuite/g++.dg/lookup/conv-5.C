// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 17 Dec 2004 <nathan@codesourcery.com>

// PR 18721. bogus error
// Origin:  Mikael Kilpel?inen <belz@kolumbus.fi>

struct A {
  template<typename T>
  operator T() const;
  
  operator float() const;
};

