// PR c++/42059
// { dg-do compile }
// { dg-options "-std=gnu++0x" }

void
foo (int i)
{
  int a[i];
  a = { }; // { dg-error "assign" }
}
