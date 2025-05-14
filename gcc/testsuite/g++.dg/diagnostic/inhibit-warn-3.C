// PR c++/118392
// { dg-do "compile" }
// { dg-additional-options "-w" }

// Fake dg-note to make sure that notes are not pruned and we can use dg-bogus.
// { dg-note "fake" "" { xfail *-*-* } }

namespace xxx {
  struct foo {
    friend void bar(); // { dg-bogus "only here as a friend" }
  };
}
void xxx::bar () {}

void foo () {}
