// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 28 Dec 2001 <nathan@codesourcery.com>

// PR 775 friend classes with qualified names inside template classes.

struct A
{
  struct B {
    B () { }
  };
};

template <class T>
struct C: A {
  friend A::B::B (); // 2.95.2 ICE
  friend struct A;
  friend struct A::B; // 2.97 error
};

template class C<char>;

template <typename T> class TPL
{
  class nested;
};

template <typename T> class TPL<T>::nested 
{
};
