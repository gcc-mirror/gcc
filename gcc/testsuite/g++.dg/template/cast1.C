// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Dec 2004 <nathan@codesourcery.com>

// PR 18949. Forgot to convert from reference.
// Origin: Volker Reichelt <reichelt@gcc.gnu.org>

struct A
{
    void foo();
};

template<int> void bar(A& a)
{
    const_cast<A&>(a).foo();
    static_cast<A&>(a).foo();
    reinterpret_cast<A&>(a).foo();
    ((A&)a).foo();
}

template void bar<0>(A& a);
