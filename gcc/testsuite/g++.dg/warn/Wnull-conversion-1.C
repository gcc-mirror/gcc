// { dg-do compile }
// { dg-options "-Wconversion-null" }

#include <stddef.h>

void func1(int* ptr);

void func2() {
  int* t = false;             // { dg-warning "converting 'false' to pointer" "" { target { ! c++11 } } }
// { dg-error "cannot convert" "" { target c++11 } .-1 }
  int* p;
  p = false;                  // { dg-warning "converting 'false' to pointer" "" { target { ! c++11 } } }
// { dg-error "cannot convert" "" { target c++11 } .-1 }
  int* r = sizeof(char) / 2;  // { dg-error "invalid conversion from" "" { target c++11 } }
  func1(false);               // { dg-warning "converting 'false' to pointer" "" { target { ! c++11 } } }
// { dg-error "cannot convert" "" { target c++11 } .-1 }
  int i = NULL;               // { dg-warning "converting to non-pointer" }
}
