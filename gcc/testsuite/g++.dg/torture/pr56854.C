// PR tree-optimization/56854
// { dg-do compile }

inline void *
operator new (__SIZE_TYPE__, void *p) throw ()
{
  return p;
}

struct A
{
  int a;
  A () : a (0) {}
  ~A () {}
  A &operator= (const A &v) { this->~A (); new (this) A (v); return *this; }
};
A b[4], c[4];

void
foo ()
{
  for (int i = 0; i < 4; ++i)
    c[i] = b[i];
}
