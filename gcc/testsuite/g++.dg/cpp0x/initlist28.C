// PR c++/42060
// { dg-do compile { target c++11 } }

void foo()
{
  int a[1];
  throw a = {}; // { dg-error "11:assigning to an array from an initializer list" }
}
