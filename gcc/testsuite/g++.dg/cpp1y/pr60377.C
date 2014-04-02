// PR c++/60377
// { dg-do compile { target c++1y } }
// { dg-options "" }

void foo(auto, void (f*)()); // { dg-error "expected" }

struct A
{
  int i;
};
