// Build don't link:

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 May 1999 <nathan@acm.org>

// Template deduction and type unification should not issue diagnostics when
// they're trying to see if it's possible.  Here deduction fails in some cases
// because you cant cv qualify a function type.

template<class T> void fn(){} // A

template<class T> void fn(T const *){} // B

// these next two specializations need to know if they're specializing A or B.
// They specialize A, because they can't instantiate B.

template<> void fn<int &>() {} // ok, specialize A

template<> void fn<void ()>() {} // ok, specialize A

// now make sure we moan when we really should
template<class T> void foo(T const *){}

void f()
{
  foo<int &>(); // ERROR - attempt to build int & const *
  foo<void ()>(); // ERROR - attempt to build void (const *)()
}

typedef void (*Fptr)();

template<class T> void PV(Fptr const &, T const * const &);
template<class T1, class T2> void PV(T1 const * const &, T2 const * const &);

void baz()
{
  void *t;
  PV(&baz, t);
}
