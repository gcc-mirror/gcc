// { dg-do assemble  }
// { dg-options "-ansi -pedantic-errors -Wsign-compare -Wno-deprecated" }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Kaveh R. Ghazi <ghazi@caip.rutgers.edu> 5/13/2001

int foo(int x, int y, unsigned u)
{
  /* A MAX_EXPR is non-negative if EITHER argument to the MAX_EXPR is
     determined to be non-negative.  */
  if (u < (x >? -1)) // { dg-warning "" } signed and unsigned
    return x;
  if (u < (x >? 10))
    return x;
  if ((10 >? x) < u)
    return x;
  if (u < (x >? (y ? (x==y) : 10)))
    return x;
  if (((y ? 10 : (x==y)) >? x) < u)
    return x;

  /* A MIN_EXPR is non-negative if BOTH arguments to the MIN_EXPR are
     determined to be non-negative.  */
  if (u < ((x?11:8) <? -1)) // { dg-warning "" } signed and unsigned
    return x;
  if (u < ((x?11:8) <? 10))
    return x;
  if ((10 <? (x?8:11)) < u)
    return x;
  if (u < ((x?11:(x==y)) <? 10))
    return x;
  if ((10 <? (x?(x==y):11)) < u)
    return x;

  return 0;
}
