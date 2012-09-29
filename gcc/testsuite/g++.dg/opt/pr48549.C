// PR rtl-optimization/48549
// { dg-do compile }
// { dg-options "-fcompare-debug -O2" }
// { dg-xfail-if "" { powerpc-ibm-aix* } { "*" } { "" } }

__extension__ typedef __PTRDIFF_TYPE__ pdiff_t;

void
foo (void *from, void *to)
{
  pdiff_t offset = reinterpret_cast <pdiff_t>(to)
		   - reinterpret_cast <pdiff_t>(from);
  if (offset != static_cast <int>(offset))
    *(int *) 0xC0DE = 0;
  reinterpret_cast <int *>(from)[1] = offset;
}
struct A
{
  A () : a () {}
  A (void *x) : a (x) {}
  void *bar () { return a; }
  void *a;
};
struct C;
struct D;
struct E : public A
{
  C m1 (int);
  D m2 ();
  E () {}
  E (A x) : A (x) {}
};
struct C : public E
{
  C () {}
  C (void *x) : E (x) {}
};
struct D : public E
{
  D (void *x) : E (x) {}
};
C
E::m1 (int x)
{
  return (reinterpret_cast <char *>(bar ()) + x);
}
D
E::m2 ()
{
  return reinterpret_cast <char *>(bar ());
}
struct B
{
  E a;
  unsigned b : 16;
  unsigned c : 1;
};
void
baz (B *x)
{
  for (unsigned i = 0; i < 64; i++)
    {
      D d = x[i].a.m2 ();
      C c = x[i].a.m1 (x[i].c);
      foo (d.bar (), c.bar ());
    }
}
