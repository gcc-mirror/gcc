// PR c++/98343
// { dg-options "-std=c++2a" }

#include "pr98343.H"

const void *ptr2 = __builtin_source_location ();
