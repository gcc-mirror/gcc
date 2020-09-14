// PR c++/60304
// { dg-do compile { target c++98_only } }
// { dg-options "-Wconversion-null" }

#include <stdbool.h>
int * foo() {return false;} // { dg-warning "converting 'false' to pointer" }
