// { dg-do run }
// { dg-options -Wold-style-cast }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 Apr 2003 <nathan@codesourcery.com>

// DR273 POD can have an operator&, offsetof is still required to work

#include <stddef.h>

struct POD1
{
  int m;
  
  void *operator& () const {return 0;} // yes, still a pod!
};

struct POD2 
{
  int m;
};

void *operator& (POD2 const &) {return 0;} // ouch!

struct POD3 
{
  int prefix;
  
  POD1 m;
};

struct POD4
{
  int prefix;
  
  POD1 m;
};

int main ()
{
  if (offsetof (POD3, m) != sizeof (int))
    return 1;
  if (offsetof (POD4, m) != sizeof (int))
    return 2;
  return 0;
}

