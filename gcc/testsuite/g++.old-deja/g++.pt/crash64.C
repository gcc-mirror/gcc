// Build don't link:

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 10 Jan 2001 <nathan@codesourcery.com>

// Bug 1546. We ICE'd trying to unify an array of unknown bound,
// checking to see if it was a variable sized array.

template <class _Tp> class allocator {};

template <class _Tp, class _Allocator>
struct _Alloc_traits
{
  static const bool _S_instanceless = false;
};

template <class _Tp, class _Tp1>
struct _Alloc_traits<_Tp, allocator<_Tp1> >
{
  static const bool _S_instanceless = true;
};

typedef char state [];
bool y = _Alloc_traits<state, allocator<state> >::_S_instanceless;
