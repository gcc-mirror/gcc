// PR c++/42060
// { dg-options "-std=c++0x" }

void foo()
{
  int a[1];
  throw a = {}; // { dg-error "assign" }
}
