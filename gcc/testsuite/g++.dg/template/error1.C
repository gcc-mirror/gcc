// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 30 Jun 2003 <nathan@codesourcery.com>

// PR c++ 10219. ICE

template <class T> void make_pair(T x);

void foo(){
  struct fps_chan_ID fps; // { dg-error "incomplete" "" }
  make_pair(fps); // { dg-error "no matching function" "" }
}
