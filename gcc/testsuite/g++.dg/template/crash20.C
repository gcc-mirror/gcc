// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 Jul 2004 <nathan@codesourcery.com>

// ICE with incompletable type.

class INC;

template <typename T> class B {};

template<typename T> void Foo (B<T> &);

void Foo (INC &);

void Baz (INC *p)
{
  Foo (*p);
}

