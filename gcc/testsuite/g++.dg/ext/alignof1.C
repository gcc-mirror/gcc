// { dg-do run }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Gabriel Dos Reis <gdr@codesourcery.com>, 2002-07-20
// Bug PR/7363.

template<typename T>
int my_alignof()
{
  return __alignof__ (T);
}

template<typename>
  struct X { };

int main()
{
  return !my_alignof<X<void> >();
}
