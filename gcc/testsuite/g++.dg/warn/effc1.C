// { dg-options -Weffc++ }
// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 Apr 2002 <nathan@codesourcery.com>

// Bug 5719 

class A
{
  public:
  A & operator+=( int );
  A & operator+( int ); // { dg-warning ".* should return by value" "" }
  A operator+=( float );
  A operator+( float );
};
