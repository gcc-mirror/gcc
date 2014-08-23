// PR c++/60377
// { dg-do compile { target c++14 } }
// { dg-options "" }

void foo(auto, void (f*)()); // { dg-error "expected" }

struct A
{
  int i;
};
