// PR target/25005
// { dg-options "-O2 -funroll-loops" }
// { dg-do compile }

inline void *operator new (__SIZE_TYPE__, void *__p) throw() { return __p; }

struct M { ~M() { } };

struct P
{
  P () { v[0] = 0; v[1] = 0; v[2] = 0; }
  P (const P &x) { for (int i = 0; i < 3; ++i) v[i] = x.v[i]; }
  double v[3];
};

struct V : public M
{
  V (const P *x, const P *y)
  {
    P *b = this->a = ::new P[2];
    for (; x != y; ++x, ++b)
      ::new (b) P(*x);
  }
  P *a;
};

void bar (const V &);

void
foo ()
{
  const P d[2] = { P(), P() };
  bar (V (&d[0], &d[2]));
}
