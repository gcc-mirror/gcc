// { dg-do assemble  }
// Based on a bug report by Stephen Vavasis <vavasis@CS.Cornell.EDU>

// declares template operator!=
#include <utility>

struct foo {
  enum e { bar } baz:1;
  void test() { 
    baz != bar;
  }
};
