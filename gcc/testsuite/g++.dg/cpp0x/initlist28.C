// PR c++/42060
// { dg-options "-std=c++11" }

void foo()
{
  int a[1];
  throw a = {}; // { dg-error "assign" }
}
