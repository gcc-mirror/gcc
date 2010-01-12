// PR tree-optimization/42645
// { dg-do compile }
// { dg-options "-fcompare-debug -O1" }

extern void foo ();

struct S
{
  struct T
  {
    int t1;
    char t2[4];
    T *t3;
  } t;
  int m1 () const { return t.t3[0].t1; }
  char *m2 () { foo (); }
  void m3 (int x) { char *m = m2 (); if (m1 () > 0 && x > 0); }
  void m4 () { if (m1 () > 0) for (int i = 0; i < 4; i++) t.t2[i] = 0; }
};

void
f (S *a)
{
  a->m3 (0);
  a->m4 ();
}
