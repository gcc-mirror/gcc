// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 23 Aug 2005 <nathan@codesourcery.com>

// PR 22454: ICE
// Origin: Volker Reichelt reichelt@gcc.gnu.org

template<int> struct A
{
    A(void* = &operator new);
};
