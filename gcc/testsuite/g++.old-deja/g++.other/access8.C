// Build don't link:
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 10 Feb 2000 <nathan@acm.org>

// Test that access to static members from a nested class of the derived
// type works.

class Base
{
  protected:
  static int Some_var;
  typedef int Some_t;
};

class Derived : Base
{
  protected:
  struct Nested
  {
    void Foo (Some_t);
    void Bar (Base::Some_t) { Base::Some_var = 1; }
  };
};

void Derived::Nested::Foo (Some_t) {
  Some_var = 2;
}
