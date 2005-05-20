// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 8 Mar 2005 <nathan@codesourcery.com>

// PR 20375: ICE
// Origin: Joseph S. Myers <jsm28@gcc.gnu.org>
// { dg-options "-mlp64" { target "ia64-*-hpux*" } }

union U
{
  void *m[7];
};

struct C;

void f(struct C *c, float f, union U, ...)
{ }
