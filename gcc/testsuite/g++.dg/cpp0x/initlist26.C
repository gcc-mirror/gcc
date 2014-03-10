// PR c++/42059
// { dg-do compile { target c++11 } }
// { dg-options "" { target { ! c++1y } } }

void
foo (int i)
{
  int a[i];
  a = { }; // { dg-error "assign" }
}
