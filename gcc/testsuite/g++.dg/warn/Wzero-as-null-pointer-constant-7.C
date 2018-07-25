// { dg-options "-Wzero-as-null-pointer-constant" }
// { dg-do compile { target c++11 } }

#include <cstddef>

void test01()
{
  char* x(NULL);
  char* x2{NULL};
  char* x3 = NULL;
  char* x4(0); // { dg-warning "zero as null pointer" }
  char* x5 = 0; // { dg-warning "zero as null pointer" }
}
