// { dg-do run  }
// { dg-options "-Wno-pmf-conversions" }
// Test conversion of pointers to virtual member functions to
// pointers to non-member functions.

struct A{
  int i;
  A () :i(1){}
  virtual void foo();
}a;

void A::foo()
{
  i = 0;
}

int main()
{
  void (*f)(A*) = (void(*)(A*))(&A::foo);
  f(&a);
  return a.i;
}
