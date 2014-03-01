// PR c++/60377
// { dg-options -std=c++1y }

void foo(auto, void (f*)()); // { dg-error "expected" }

struct A
{
  int i;
};
