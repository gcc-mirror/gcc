// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 4 February 2001 <nathan@codesourcery.com>

// Check constructor vtables work.
// Additional files: vtable3.h

#define A_EMPTY
#define B1_EMPTY
#define B2_EMPTY
#define C_EMPTY
#define C_PARENTS virtual B1, virtual B2

#include "vtable3.h"
