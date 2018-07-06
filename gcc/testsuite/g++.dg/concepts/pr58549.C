// PR c++/58549
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

void foo(auto)
{
  void bar();
}
