// { dg-do run }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Sep 2002 <nathan@codesourcery.com>

// PR 7768 template dtor pretty function wrong

#include <string.h>

static size_t current = 0;
static bool error = false;

static char const *names[] =
{
  "X<T>::X() [with T = void]",
  "X<T>::~X() [with T = void]",
  0
};

void Verify (char const *ptr)
{
  error = strcmp (ptr, names[current++]);
}
  
template <typename T>
struct X
{
  X() { Verify (__PRETTY_FUNCTION__); }
  ~X() { Verify (__PRETTY_FUNCTION__); }
};

int main()
{
  {
    X<void> x;
    
    if (error)
      return current;
  }
  if (error)
    return current;
  return 0;
}
