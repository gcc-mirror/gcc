// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 6 Jan 2003 <nathan@codesourcery.com>

// PR 9212. We erroneously accepted an ill-formed
// function-declaration, rather than a variable initializer.


struct A
{
    enum E { e };
    A(E);
};

struct B
{
    enum F { f };
    B(F);
};

struct C
{
    C(A, B, A);
};

C c(A(A::e), B(B::f), A(A::e)); // This is not a function declaration
