// PR c++/58548
// { dg-do compile { target c++1y } }
// { dg-options "" }

void foo(auto)
{
  struct A { int i; };
}
