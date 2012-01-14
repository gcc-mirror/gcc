//PR c++/50012
// { dg-options "-Wsign-compare" }

int foo(unsigned int *a, int b)
{
  return (*a) <= b; // { dg-warning "comparison between signed and unsigned" }
}

int bar(unsigned int *a, int b)
{
  return *a <= b; // { dg-warning "comparison between signed and unsigned" }
}
