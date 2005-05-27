// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 May 2005 <nathan@codesourcery.com>

// Origin:Volker Reichelt reichelt@gcc.gnu.org
// PR 21681. ICE with inappropriate access check.

template<int X> struct A;

struct B
{
    template<int N> void foo()
    {
        A<N>::X::Y;
    }
};
