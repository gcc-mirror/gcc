// PR c++/39681
// { dg-do compile { target c++11 } }

struct A
{
  int* p = new foo; // { dg-error "16:expected type-specifier" }
};
