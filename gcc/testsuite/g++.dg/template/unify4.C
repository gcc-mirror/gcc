// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 28 Jan 2003 <nathan@codesourcery.com>

// PR 9437. We'd unify 'T *' with 'U C::*', which is obviously broken

struct X
{
  template <typename T>
  operator T* () const { return static_cast<T*> (0); }
} null;

struct A { int i; };

static void f (int A::* pmi) { }

int main () { f (null); } // { dg-error "cannot convert" }
