// PR c++/96364
// { dg-do compile { target c++14 } }

auto a[[]] [[]]();
auto a() {}

void v[[]] [[]]();
void v() {}

void g()
{
  v();
  return a();
}
