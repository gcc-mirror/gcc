// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 Feb 2001 <nathan@codesourcery.com>

// Bug 1981. using declarations in namespace scope were not remembered.

namespace A
{
  void swap () {}
};

template <class T> void f()
{
  using A::swap;
}

template void f<float> ();

int foo (int) { return 0;}

namespace B
{
  int foo (int) { return 1;}
  
  template <class T> int baz ()
  {
    using ::foo;
    
    return foo (1);
  }
  template int baz<float> ();
};

int main ()
{
  return B::baz<float> ();
}
