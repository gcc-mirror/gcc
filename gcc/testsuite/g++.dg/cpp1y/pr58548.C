// { dg-do compile }
// { dg-options "-std=gnu++1y" }

// PR c++/58548

void foo(auto)
{
  struct A { int i; };
}

