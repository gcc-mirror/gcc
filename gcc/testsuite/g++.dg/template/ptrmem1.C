// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 31 Dec 2001 <nathan@codesourcery.com>

// PR 3716 tsubsting a pointer to member function did not create a
// pointer to member function.

template <class C, class T, T C::*M>
struct Closure
{
  T operator() (C & c) const { return (c.*M); }
};

template <class C, class T, T (C::* M)()>
struct Closure<C, T (), M>
{
  T operator()(C & c) const { return (c.*M)(); }
};

struct A
{
  int get();
};
  
static Closure<A, int (), & A::get> get_closure;


void Foo ()
{
  A a;
  get_closure (a);
}
