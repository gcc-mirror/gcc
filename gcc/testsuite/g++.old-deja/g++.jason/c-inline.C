// { dg-do assemble  }
// Bug: the compiler gets hopelessly confused.

#line 1 "c-inline.h"
#pragma interface
inline double abs (double) { return 0.0; }
inline short abs (short) { return 0; }
#line 2 "c-inline.C"
extern "C" {
  inline int abs (int) { return 0; } // causes segfault - 
}
