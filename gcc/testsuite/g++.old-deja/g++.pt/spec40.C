// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 12 Feb 2001 <nathan@codesourcery.com>

// More from bug 1617. We didn't resolve partial ordering properly. The
// std is rather vague about it anyway, DR 214 talks about this.

template <typename T> int Foo (T const *) {return 1;}
template <unsigned I> int Foo (char const (&)[I]) {return 2;}

int main ()
{
  return Foo ("a") != 2;
}
