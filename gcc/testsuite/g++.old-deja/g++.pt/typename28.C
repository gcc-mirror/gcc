// Build don't link:
//
// Origin: Jens.Maurer@gmx.net
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 11 Apr 2001 <nathan@codesourcery.com>

// Bug 1844. We can meet types in cp_tree_equal via a template-id-expr.

typedef int *Ptr;

template<class T> struct B
{
  typedef typename T::template X<T> type;
  typedef typename T::template X<Ptr> type2;
  typedef typename T::template X<int *> type3;
  
  void foo (type);
  void baz (type2);
  
};

template<class T> void B<T>::foo (type)
{
}
template<class T> void B<T>::baz (type3)
{
}
