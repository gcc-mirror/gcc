// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 31 Dec 2001 <nathan@codesourcery.com>

// PR 5132. ICE on struct constructors in templates.

// snippets from bits/huge_val.h

#define __HUGE_VAL_bytes        { 0, 0, 0, 0, 0, 0, 0xf0, 0x7f }
#define __huge_val_t    union { unsigned char __c[8]; double __d; }
#define HUGE_VAL       (__extension__ \
  ((__huge_val_t) { __c: __HUGE_VAL_bytes }).__d)

void foo( const int&) {
  HUGE_VAL; // no problem here
}

template <class F>
void Tfoo( const F&) {
  HUGE_VAL; // g++ fails here
}

template <typename T> struct M { T m; };

void Foo ()
{
  Tfoo (1.2f);
  (__extension__ ((M<int>) {m:3}));
  (__extension__ ((M<short> []) {{m:3}}));
}
