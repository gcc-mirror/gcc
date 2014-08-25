// PR c++/58548
// { dg-do compile { target c++14 } }
// { dg-options "" }

void foo(auto)
{
  struct A { int i; };
}
