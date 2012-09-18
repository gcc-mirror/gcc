// PR c++/53210
// { dg-options "-Wall" }

struct S
{
  S(int i) : j(j) { }  // { dg-warning "is initialized with itself" }
  int j;
};
