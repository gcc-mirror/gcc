// { dg-do assemble  }
// { dg-options "-ansi -pedantic-errors -Winline -O1" }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 9 Mar 2000 <nathan@codesourcery.com>

// derived from a bug report by Benjamin Kosnik <bkoz@cygnus.com>

// __FUNCTION__ was erroneously causing us to issue a `cannot inline'
// diagnostic, even though we'd (a) inlined it, (b) been forced to issue an
// out of line body by taking it's address, (c) not used __FUNCTION__.

inline void wibble ()
{}

inline void wobble ()
{}                          // { dg-bogus "" } cannot inline

void bar (void (*)());

void bar1 ()
{
  wibble ();                // can be inlined
  void (*ptr) () = wobble;  // force out of line issue
  
  bar (ptr);                // make sure we make use of it
}

struct B
{
  void mwibble ()
  {};
  void mwobble ()
  {};                       // { dg-bogus "" } cannot inline
  
  static void swibble ()
  {};
  static void swobble ()
  {};                       // { dg-bogus "" } cannot inline
};

void bar (void (B::*)());

void bar2 ()
{
  B::swibble ();                  // can be inlined
  void (*ptr) () = &B::swobble;   // force out of line issue
  
  bar (ptr);                      // make sure we make use of it
}

void bar3 (B *b)
{
  b->mwibble ();                    // can be inlined
  void (B::*ptr) () = &B::mwobble;  // force out of line issue
  
  bar (ptr);                        // make sure we make use of it
}

struct C
{
  virtual void vwobble ()
  {};                               // { dg-bogus "" } cannot inline
};

void bar4 ()
{
  C c;                              // force issue of C's vtable etc
}
