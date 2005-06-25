// Copyright (C) 2001, 2002 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 2, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING.  If not, write to the Free
// Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
// USA.

// Contributed by Gabriel Dos Reis <gdr@codesourcery.com>

// { dg-do compile }

namespace N
{
  template<int> void f() {}
}

using N;             // { dg-error "(using-declaration)|(nested-name)" "" }
using ::N;           // { dg-error "using-declaration" "" }
using N::f< 0 >;     // { dg-error "using-declaration" "" }

struct  A {
  template <class T> void f(T);
  template <class T> struct X { };
}; 

struct B : A {
  using A::X;        // OK
  using A::f;        // OK
};

struct C : A {
  using A::f<double>; // { dg-error "using-declaration" "" }
  using A::X<int>;    // { dg-error "using-declaration" "" }
};

