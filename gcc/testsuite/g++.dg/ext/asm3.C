// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 16 Sep 2002 <nathan@codesourcery.com>

// PR 7015. ICE with asms

int two(int in)
{
  register int out;
  __asm__ ("" : "r" (out) : "r" (in)); // { dg-error "output operand" "" }
  return out;
}
