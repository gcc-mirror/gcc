// { dg-do compile }
// { dg-options "-fabi-version=1 -Wabi" }

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

void f (S<Foo>){} // { dg-warning "mangled name" }
// { dg-final { scan-assembler "\n_?_Z1f1SIXadL3FooEEE\[: \t\n\]" } }

void g (T<&NMS::V>){}  // { dg-warning "mangled name" }
// { dg-final { scan-assembler "\n_?_Z1g1TIXadL_ZN3NMS1VEEEE\[: \t\n\]" } }
