// { dg-do compile }

// Copyright (C) 2007 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 Jul 2007 <nathan@codesourcery.com>

// Origin: Danny Boelens <danny.boelens@artwork-systems.com>
// PR 32839.  Default arguments propagated through the type system to
// an indirect call.

template<typename T>
struct TPL
{
  enum Whatever {e1, e2};
 
  static void Quux (int i = e1 | e2);
};

template <typename F>
void DoIt (F fun)
{
  fun (); // { dg-error "too few arguments" }
}

void Foo ()
{
  DoIt (&TPL<int>::Quux);
}
