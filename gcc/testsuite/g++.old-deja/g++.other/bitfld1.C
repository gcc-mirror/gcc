// Build don't link:
// Based on a bug report by Stephen Vavasis <vavasis@CS.Cornell.EDU>

// excess errors test - XFAIL *-*-*

// declares template operator!=
#include <utility>

struct foo {
  enum e { bar } baz:1;
  void test() { 
    baz != bar;
  }
};
