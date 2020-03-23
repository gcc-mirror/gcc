// PR tree-optimization/94224
// { dg-do compile }
// { dg-options "-O2 -fnon-call-exceptions -Wno-return-type" }

void foo (int, int, long);

static inline int
bar (int &x)
{
  x = 0;
}

struct U
{
  int n, p;
  long q;
  bool *z;
  int a;
  U () : n (), z (), a (1) {}
  ~U () { if (n) foo (p, n, q); }
  void baz () { bar (a); }
};

struct W
{
  U w[2];
  W () { w[0].baz (); }
};

void
qux ()
{
  new W;
}
