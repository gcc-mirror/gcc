// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Aug 2002 <nathan@codesourcery.com>

// PR c++ 7598, offsetof broke
// PR c++ 11072, DR 273's solution is broken

#include <cstddef>

struct F
{
  char i;
  char j;
};

static int ary[offsetof(F, j)];
