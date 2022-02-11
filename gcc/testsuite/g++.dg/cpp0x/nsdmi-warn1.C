// PR c++/103347
// { dg-do compile { target c++11_down } }

#include "nsdmi-warn1.h"

struct A {
  void *x = NULL; // { dg-error "11:only available" "" { target c++98_only } }
  void *y{NULL}; // { dg-error "only available|extended initializer" "" { target c++98_only } }
  int z = 1 + 2; // { dg-error "9:only available" "" { target c++98_only } }
};
