// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Dec 2004 <nathan@codesourcery.com>

// PR 18981. ICE
// Origin:  Andreas Schwab <schwab@suse.de>

template <class T> 
struct tree { 
  struct iterator; 
  struct sibling_iterator { 
    friend struct tree<T>::iterator; 
  }; 
}; 
