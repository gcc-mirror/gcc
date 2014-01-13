// PR c++/58548
// { dg-do compile }
// { dg-options "-std=gnu++1y" }

void foo(auto)
{
  struct A { int i; };
}
