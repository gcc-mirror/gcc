// PR c++/42059
// { dg-do compile }
// { dg-options "-std=gnu++11" }

void
foo (int i)
{
  int a[i];
  a = { }; // { dg-error "assign" }
}
