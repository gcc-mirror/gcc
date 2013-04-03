// { dg-options "-std=c++11 -Wshadow" }

struct A
{
  int i;
  void f()
  {
    [=]{ int i; };		// { dg-warning "shadows" }
  }
};
