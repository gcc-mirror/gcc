// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 30 June 2003 <nathan@codesourcery.com>

// PR c++ 11072, DR 273's solution is broken

#include <stddef.h>

struct F
{
  char i;
  char j;
};

static int ary[offsetof (F, j)];

