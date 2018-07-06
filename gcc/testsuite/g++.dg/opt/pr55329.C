// PR tree-optimization/55329
// { dg-do compile }
// { dg-options "-O -fno-guess-branch-probability -fnon-call-exceptions --param=early-inlining-insns=111" }

void *f1 ();
void f2 (void *);
void f3 ();
static inline void *
f4 ()
{
  void *p = f1 ();
  if (!p)
    f3 ();
  return p;
}

struct A
{
  int *a;
  A ();
  ~A () { a3 (); }
  int a1 (int * p) { if (!p) f3 (); f2 (p); return 0; }
  int *a2 ();
  void a3 () { if (*a) a1 (a); }
  int a4 (int x) { if (*a) f4 (); *a2 () += x; return 0; }
};

struct B : A
{
  ~B () { a3 (); }
};

template <class T>
struct C
{
  T *c;
  C ();
  int c1 () { return *(int *) f4 (); }
  ~C () { if (c1 ()) for (T *t = c + c2 (); t != c; t--) T (); }
  int c2 ();
};

class D
{
  C <C <int> > c;
};

struct E
{
  int *e;
  ~E () { delete e; }
};

struct F
{
  int *f1 ();
  D f2;
  E f3;
  F () { f4 (); }
};

struct G : F
{
  B g;
  G () { g.a4 (*g1 ()->f1 ()); g1 ()->f1 (); }
  F *g1 ();
};

void
foo ()
{
  G g;
}
