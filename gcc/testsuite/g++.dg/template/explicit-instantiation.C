// Contributed by Gabriel Dos Reis <gdr@codesourcery.com>
// Origin: Jens.Maurer@gmx.net
// { dg-do compile }

// Fixed: PR 3381

namespace N
{
  template<class T>
  class A { };
}

template class ::N::A<int>;



