// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Matt Austern 12 Sep 2002 <austern@apple.com>

// Make sure that we can pass a cast-expression as an argument that's
// passed to a function template by const reference.

template <class T>
void bar (const T&)
{ }

void foo (int x)
{
  bar ((long) x);
}
