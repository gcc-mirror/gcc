// PR c++/41876

struct A;

void foo()
{
  try {} catch(int A) {}
}
