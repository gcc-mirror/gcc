// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 25 Nov 2000 <nathan@codesourcery.com>

// We lost information about which base wasn't an aggregate type, plus we
// allowed cv qualifed bases via typedefs.

typedef int I;
typedef int cI;

struct A {};

typedef const A cA;
typedef A pA;

struct B : I {};  // ERROR - not an aggregate
struct C : cI {}; // ERROR - not an aggregate
struct D : cA {}; // ERROR - cv qualified
struct E : pA {};
