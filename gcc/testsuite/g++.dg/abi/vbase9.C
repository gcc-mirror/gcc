// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 17 Mar 2002 <nathan@codesourcery.com>
// Origin: Jakub Jelinek <jakub@redhat.com>

// PR 5681. ICE in build_secondary_vtable

struct A {
  virtual int f1 ();
};

struct B : virtual A {};

struct C {
  virtual int f2 ();
};

struct E : A {};

struct D : E,  B {};

struct F : virtual D {};

struct G : virtual F,  C {};

struct H : virtual F {};

struct I : G,  H {};
