// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 Sep 2003 <nathan@codesourcery.com>
// Origin Volker Reichelt reichelt@igpm.rwth-aachen.de

// PR 11922

struct A
{
  template <bool> struct B;
  struct C;
};

template <> struct A::B<false> {};

template <typename T> void foo()
{
  T::C (); // { dg-error "parsed as a non-type|if a type is meant" }
  T::template B<false>(); // { dg-error "parsed as a non-type" "non-type" }
			  // { dg-message "if a type" "if a type" { target *-*-* } 20 }
}

void bar()
{
  foo<A>(); // { dg-message "required" }
}
