// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 30 Nov 2004 <nathan@codesourcery.com>

// PR 17431. copy ctor from user conv
// Origin: Volker Reichelt <reichelt@gcc.gnu.org>

struct A {};

struct B : A
{
  B(int);
  B(A);
};

void foo(B);

void bar()
{
  foo(0);
}
