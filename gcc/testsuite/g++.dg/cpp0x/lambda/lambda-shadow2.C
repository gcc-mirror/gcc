// { dg-do compile { target c++11 } }
// { dg-options "-Wshadow" }

struct A
{
  int i;
  void f()
  {
    [=]{ int i; };		// { dg-warning "shadows" }
  }
};
