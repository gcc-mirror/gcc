// { dg-do run  }
// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 Nov 1999 <nathan@acm.org>

// make sure __FUNCTION__ and __PRETTY_FUNCTION__ work in templates

#include <stdio.h>
#include <string.h>

static bool bad = false;

template<class T> void f1 (T)
{
  char const *function = __FUNCTION__;
  char const *pretty = __PRETTY_FUNCTION__;
  
  printf ("generic\n");
  printf ("__FUNCTION__ %s\n", function);
  printf ("__PRETTY_FUNCTION__ %s\n", pretty);
  
  if (strcmp (function, "f1"))
    bad = true;
  if (strcmp (pretty, "void f1(T) [with T = float]")) // only for float instantiation
    bad = true;
}

template<> void f1<int> (int)
{
  char const *function = __FUNCTION__;
  char const *pretty = __PRETTY_FUNCTION__;
  
  printf ("specialized\n");
  printf ("__FUNCTION__ %s\n", function);
  printf ("__PRETTY_FUNCTION__ %s\n", pretty);
  
  if (strcmp (function, "f1<int>"))
    bad = true;
  if (strcmp (pretty, "void f1(T) [with T = int]"))
    bad = true;
}

int main ()
{
  f1(0);    // f1<int>
  f1(0.0f); // f1<float>
  return bad;
}
