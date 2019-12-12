// { dg-do compile }
// { dg-options "-O0" }
// { dg-final { scan-assembler-not "\n_?_ZN1X3FooIiEEvT_\[: \t\n\]" } }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Mar 2003 <nathan@codesourcery.com>

// PR 10047. bogus warning.

struct X 
{
  template <typename T> static void Foo (T)  {}
};

extern template void X::Foo<int> (int); // extern, so don't emit it

int main () {
  X::Foo (1);  // ok, we've seen the defn
}

