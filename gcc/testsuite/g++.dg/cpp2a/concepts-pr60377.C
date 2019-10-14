// PR c++/60377
// { dg-do compile { target c++14 } }

void foo(auto, void (f*)()); // { dg-error "auto|expected" }

struct A
{
  int i;
};
