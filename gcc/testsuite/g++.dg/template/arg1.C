// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 Mar 2003 <nathan@codesourcery.com>

// PR 9978. We rejected a constant expression.
  
enum { val = 1 };

template <class T>
struct Bar
{
  static const int A = val;
  static const int B = A + 1;
};
