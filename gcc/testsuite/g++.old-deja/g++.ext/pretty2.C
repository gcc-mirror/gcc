// Copyright (C) 1999, 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 Nov 1999 <nathan@acm.org>

// make sure __FUNCTION__ and __PRETTY_FUNCTION__ work in member functions

#include <stdio.h>
#include <string.h>

static bool bad = false;

struct X
{
  X ();
  ~X ();
  void fn ();
  operator int ();
};

X::X ()
{
  char const *function = __FUNCTION__;
  char const *pretty = __PRETTY_FUNCTION__;
  
  printf ("ctor\n");
  printf ("__FUNCTION__ %s\n", function);
  printf ("__PRETTY_FUNCTION__ %s\n", pretty);
  
  if (strcmp (function, "X"))
    bad = true;
  if (strcmp (pretty, "X::X()"))
    bad = true;
}
X::~X ()
{
  char const *function = __FUNCTION__;
  char const *pretty = __PRETTY_FUNCTION__;
  
  printf ("dtor\n");
  printf ("__FUNCTION__ %s\n", function);
  printf ("__PRETTY_FUNCTION__ %s\n", pretty);
  
  if (strcmp (function, "X"))
    bad = true;
  if (strcmp (pretty, "X::~X()"))
    bad = true;
}
void X::fn ()
{
  char const *function = __FUNCTION__;
  char const *pretty = __PRETTY_FUNCTION__;
  
  printf ("member fn\n");
  printf ("__FUNCTION__ %s\n", function);
  printf ("__PRETTY_FUNCTION__ %s\n", pretty);
  
  if (strcmp (function, "fn"))
    bad = true;
  if (strcmp (pretty, "void X::fn()"))
    bad = true;
}
X::operator int ()
{
  char const *function = __FUNCTION__;
  char const *pretty = __PRETTY_FUNCTION__;
  
  printf ("conversion\n");
  printf ("__FUNCTION__ %s\n", function);
  printf ("__PRETTY_FUNCTION__ %s\n", pretty);
  
  if (strcmp (function, "operator i"))
    bad = true;
  if (strcmp (pretty, "X::operator int()"))
    bad = true;
  return 0;
}

int main ()
{
  {
    X x;
    
    x.fn ();
    (void)int (x);
  }
  return bad;
}
