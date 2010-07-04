// PR tree-optimization/44182
// { dg-do compile }
// { dg-options "-fcompare-debug" }

struct S
{
  int i;
  S ();
  ~S ();
  void f1 ();
  void f2 (S s)
  {
    f3 (s.i);
    for (int j = 0; j < s.i; j++) f1 ();
  }
  void f3 (int j)
  {
    if (j > i) f1 ();
  }
};

void
f (S *x)
{
  x->f2 (S ());
}
