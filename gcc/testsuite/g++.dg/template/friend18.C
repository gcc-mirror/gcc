// { dg-do run }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 Mar 2003 <nathan@codesourcery.com>

// PR 10158. implicit inline template friends ICE'd

template <int N> struct X
{
  template <int M> friend int foo(X const &)
  {
    return N * 10000 + M;
  }
};
X<1234> bring;

int main() {
  return foo<5678> (bring) != 12345678;
}
