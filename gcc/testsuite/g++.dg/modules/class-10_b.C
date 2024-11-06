// { dg-additional-options "-fmodules-ts -Wno-pedantic" }
// Test bits and pieces of merging information
// from class defs into forward declarations

struct Align;
struct Final;
struct NeedsConstructing;

import "class-10_a.H";

static_assert(alignof(Align) == 16);

struct TestFinal : Final {};  // { dg-error "cannot derive" }

struct TestNeedsConstructing {
  struct {
    NeedsConstructing a;  // { dg-error "with constructor not allowed in anonymous aggregate" }
  };
};
