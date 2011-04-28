// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 25 Nov 2000 <nathan@codesourcery.com>

// We lost information about which base wasn't an aggregate type.

typedef int I;
typedef int cI;

struct A {};

typedef const A cA;
typedef A pA;

struct B : I {};  // { dg-error "" } not an aggregate
struct C : cI {}; // { dg-error "" } not an aggregate
struct D : cA {}; // cv-qualified is fine per DR 484
struct E : pA {};
