// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 23 Sep 2004 <nathan@codesourcery.com>

// Origin: Wolfgang Bangerth <bangerth@dealii.org>
// Bug 17620. Bogus duplicate base error.

struct S {}; 
 
typedef S B; 
 
struct D1 : B {}; 
struct D2 : B {}; 
