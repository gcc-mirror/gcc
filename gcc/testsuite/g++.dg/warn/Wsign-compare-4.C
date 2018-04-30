//PR c++/50012
// { dg-options "-Wsign-compare" }

int foo(unsigned int *a, int b)
{
  return (*a) <= b; // { dg-warning "comparison of integer expressions of different signedness" }
}

int bar(unsigned int *a, int b)
{
  return *a <= b; // { dg-warning "comparison of integer expressions of different signedness" }
}
