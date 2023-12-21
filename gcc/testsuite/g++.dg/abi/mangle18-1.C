// { dg-do compile }
// { dg-options "-fabi-version=2 -fabi-compat-version=2" }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 30 Nov 2003 <nathan@codesourcery.com>

// PR 13241
// mangled template arguments that are external objects incorrectly

extern "C" void Foo ();
namespace NMS 
{
  extern "C" int V;
}

template <void (*)()> struct S {};
template <int *> struct T {};

void f (S<Foo>){}
// { dg-final { scan-assembler "\n_?_Z1f1SIXadL_Z3FooEEE\[: \t\n\]" } }

void g (T<&NMS::V>){}
// { dg-final { scan-assembler "\n_?_Z1g1TIXadL_Z1VEEE\[: \t\n\]" } }
