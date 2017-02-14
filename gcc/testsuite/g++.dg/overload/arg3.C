// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 30 Nov 2004 <nathan@codesourcery.com>

// PR 17431. copy ctor from user conv
// Origin: Volker Reichelt <reichelt@gcc.gnu.org>

struct A {};

struct B : A
{
  B(int);
  B(B&);  // { dg-message "note" "" { target c++14_down } }
};

void foo(B);			// { dg-message "initializing" "" { target c++14_down } }

void bar()
{
  foo(0); // { dg-error "" "" { target c++14_down } }
}
