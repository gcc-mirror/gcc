// { dg-do run  }
// { dg-additional-files "vtable3.h" }

// Copyright (C) 2000, 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 February 2001 <nathan@codesourcery.com>

// Check constructor vtables work.

#define A_EMPTY
#define B1_EMPTY
#define B2_EMPTY
#define C_PARENTS B1, B2

#include "vtable3.h"
