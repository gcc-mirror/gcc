// PR debug/46583
// { dg-do compile }
// { dg-options "-O -fno-inline -fipa-cp -fipa-cp-clone -fcompare-debug" }

template < typename = unsigned long >struct A
{
  unsigned long elems[3];
  unsigned long *begin ()
  {
    return 0;
  }
};

void
bar (unsigned long *a1, unsigned long, unsigned long *a3, unsigned const &)
{
  *a3 = *a1;
}

A < >operatorM (A < >a1, unsigned long a2)
{
  typedef A < >G;
  G a3;
  bar (a1.begin (), a2, a3.begin (), 0);
  return a3;
}

struct B
{
  B (A < >m):n (operatorM (m, 1))
  {
  }
  A < >n;
};

void
foo ()
{
  B (A < >());
}
