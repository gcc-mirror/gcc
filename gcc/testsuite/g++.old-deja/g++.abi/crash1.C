// Build don't link:
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 11 Apr 2001 <nathan@codesourcery.com>

// Bug 1944. We failed to calculate nearly emptiness properly, and
// lost primary too.

struct A1 {};
struct A2 {};

struct B1 : virtual A1 {};
struct B2 : virtual A2 {};

struct C1 : virtual B2 {};
struct C2 : virtual B2 {};

struct D1 : virtual C1, virtual C2 {};
struct D2 : virtual C2, virtual B1 {};

struct E : virtual D1, virtual D2 {};
