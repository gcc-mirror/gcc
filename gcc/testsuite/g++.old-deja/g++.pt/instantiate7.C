// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation
// Contributed by Nathan Sidwell 6 July 2000 <nathan@codesourcery.com>

template <class T>
void Wibble (void (*fn) (), T *const __restrict__ &p2)
{}
template<class T1, class T2>
void Wibble (T1 *const __restrict__ &p1, T2 *const __restrict__ &p2)
{}

void Baz ();

void Foo (void const *ptr)
{
  Wibble (&Baz, ptr);
}
