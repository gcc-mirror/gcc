// PR c++/96721
// { dg-do run }
// { dg-options "-O2 -flifetime-dse" }

typedef int *T;

int
main ()
{
  T a = T ();
  a.~T ();
}
