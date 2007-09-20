// PR c++/33459

union A
{
  int &i; // { dg-error "may not have reference type" }
};

void foo()
{
  A();
}
