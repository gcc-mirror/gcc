// Build don't link:
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 Sep 2000 <nathan@codesourcery.com>

// Bug 512. Conversion operator functions in template_id's were always
// being looked up in global scope.

class C
{
public:

  template <typename T>
  void f () {}

  template<typename T>
  operator int ()
  { return 0;
  }
};

template void C::f <int>();

template C::operator int<float> ();

template C::operator int<double> ();

typedef int (C::* ptrmem_t) ();

template<ptrmem_t U, ptrmem_t V>
void foo ()
{
}

template void 
foo<&C::operator int<float>, &C::operator int<double> > ();
