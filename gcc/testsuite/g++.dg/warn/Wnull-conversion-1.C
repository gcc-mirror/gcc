// { dg-do compile }
// { dg-options "-Wconversion-null" }

#include <stddef.h>

void func1(int* ptr);

void func2() {
  int* t = false;             // { dg-warning "converting 'false' to pointer" }
  int* p;
  p = false;                  // { dg-warning "converting 'false' to pointer" }
  int* r = sizeof(char) / 2;
  func1(false);               // { dg-warning "converting 'false' to pointer" }
  int i = NULL;               // { dg-warning "converting to non-pointer" }
}
