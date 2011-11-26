// PR rtl-optimization/49912
// { dg-do compile }
// { dg-require-effective-target freorder }
// { dg-options "-O -freorder-blocks-and-partition" }

int foo (int *);

struct S
{
  int *m1 ();
  S (int);
   ~S () { foo (m1 ()); }
};

template <int>
struct V
{
  S *v1;
  void m2 (const S &);
  S *base ();
};

template <int N>
void V<N>::m2 (const S &x)
{
  S a = x;
  S *l = base ();
  while (l)
    *v1 = *--l;
}

V<0> v;

void
foo ()
{
  v.m2 (0);
}
