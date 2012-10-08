// PR c++/17542
// Test that we warn when an attribute preceding the class-key is ignored.
// { dg-do compile { target c++11 } }

[[gnu::packed]] struct A // { dg-warning "attribute" }
{
  char c;
  int x;
  void f();
};
