// PR c++/42060
// { dg-do compile { target c++11 } }

void foo()
{
  int a[1];
  throw a = {}; // { dg-error "assign" }
}
