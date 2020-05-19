// { dg-do run }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 Mar 2003 <nathan@codesourcery.com>

// PR 10158. implicit inline template friends ICE'd

typedef int int32_t __attribute__((mode (__SI__)));

template <int32_t N> struct X
{
  template <int32_t M> friend int32_t foo(X const &, X<M> const&)
  {
    return N * 10000 + M;
  }
};
X<1234> bring;
X<5678> brung;

int main() {
  return foo (bring, brung) != 12345678;
}
