// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 11 Jan 2001 <nathan@codesourcery.com>

// Bug 1551. We were accessing some uninitialized memory, causing us
// to reject this.

template <typename T>
struct base
{
base();
base(unsigned);
};

template <typename V>
struct Y
{
Y(unsigned = 0);
};

template <>
Y<char>::Y(unsigned) { }

base<double> x;
