// Build don't link:

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 5 Jun 2001 <nathan@codesourcery.com>

// Bug 2929. We were forgetting about template parm scope when
// injecting a friend decl into a class template specialization's
// containing scope.

template <class Type> class Vec;

template <> class Vec<double>
{
public:
  Vec ();
  Vec<double> & Fn (double);
  friend Vec<double> Fn (const Vec<double> &, double);
}; // pop_binding ICE

template <class _Tp> class Alloc
{
  template <class _Tp1> struct Rebind
  {
    typedef Alloc<_Tp1> other;
  };
};
