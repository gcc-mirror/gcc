// PR c++/56815
// { dg-options "-Wpointer-arith" }

int main()
{
  void *pv = 0;
  pv++;    // { dg-warning "3:ISO C\\+\\+ forbids incrementing a pointer" }

  typedef void (*pft) ();

  pft pf = 0;
  pf++;    // { dg-warning "3:ISO C\\+\\+ forbids incrementing a pointer" }
}
