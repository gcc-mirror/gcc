// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 28 Jan 2003 <nathan@codesourcery.com>

// PR 3902. More type/decl confusion.

template <class T>
struct S
{
  S foo (T (T));
  S foo (T(const T&));
};

int main ()
{
  S<int> (S<int>::*pf1)(int (int)) = &S<int>::foo;
  S<int> (S<int>::*pf2)(int (const int&)) = &S<int>::foo;
}
