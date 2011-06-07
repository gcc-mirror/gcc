// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 9 Jul 2003 <nathan@codesourcery.com>

// PR c++ 9483.  accepted fields with same name as class

struct test
{
  char test;  // { dg-error "with same name as class" "" }
  test();
};

template <typename T> struct X
{
  char X;  // { dg-error "with same name as class" "" }
  X ();
};

template <> struct X<int> {
  char X;  // { dg-error "with same name as class" "" }
  X();
};

X<float> i; // { dg-message "required from" "" }
