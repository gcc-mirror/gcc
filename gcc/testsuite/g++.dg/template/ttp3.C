// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Dec 2001 <nathan@codesourcery.com>

// PR 5213. We failed to spot that class List was a template, rather
// than a non-template or specialization


template <class T> class vector { };

class OUTER {
  public:
  template <class T>
  class List { };
  
  vector<class List> data; // { dg-error "invalid|required|ISO C" "" }
};

template <class T>
class List { };

// This next line should just do a lookup of 'class List', and then
// get a type/value mismatch. Instead we try and push 'class List'
// into the global namespace and get a redeclaration error.
vector<class List > data;	// { dg-error "invalid|required|declaration" "" }
