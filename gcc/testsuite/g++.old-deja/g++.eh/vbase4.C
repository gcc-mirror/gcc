
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 8 Mar 2000 <nathan@codesourcery.com>

// Derived from PR#7

// We need to destroy the thrown object when exiting the catch
// clause. That needs to destroy the original thrown object, not
// the caught one (which might be a base).

static int ok = 0;

struct A
{
  A (){};
  virtual ~A () {};
};

struct B : virtual A
{
  int value;
  B ()
    :value(10)
    {}
  ~B()
  {
    if (value == 10)
      ok = 1;
  }
};

int main()
{
  try {
    throw B ();
  } catch (A & e) {
  }
  return !ok;
}
