// PR c++/17542
// Test that we warn when an attribute preceding the class-key is ignored.
// { dg-do compile }

__attribute__ ((packed)) struct A
{				// { dg-warning "attribute" }
  char c;
  int x;
  void f();
};
