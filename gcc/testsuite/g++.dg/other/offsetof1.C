// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Aug 2002 <nathan@codesourcery.com>

// PR c++ 7598, offsetof broke

struct F
{
  char i;
  char j;
};

static int ary[((__SIZE_TYPE__)&((struct F *)0)->j)];
