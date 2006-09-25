// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 10 Aug 2005 <nathan@codesourcery.com>

// PR 23219, ICE
// Origin:Andrew Pinski <pinskia@gcc.gnu.org>
//        Volker Reichelt <reichelt@gcc.gnu.org>

template <class _Tp> class insert_iterator<slist<_Tp> > {}; // { dg-error "not a template|not declared in this scope|expected unqualified-id|extra" }
template <class _Value> class insert_iterator<int > { // { dg-error "template" }
  hash_set<_Value>;
};

template<int> struct A<X<> > {}; // { dg-error "not a template|not declared in this scope|expected unqualified-id|extra" }
struct A {};
