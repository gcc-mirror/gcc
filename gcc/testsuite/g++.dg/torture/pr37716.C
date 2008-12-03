// PR tree-optimization/37716
// { dg-do compile }

struct A
{
  struct B
  {
    int a, b, c, d;
    void *e[1];
  };
  B *d;
  inline void **f1 (int i) const
  {
    return d->e + d->c + i;
  }
};

template <typename T>
struct C
{
  struct D
  {
    void *v;
    inline T & f3 ()
    {
      return *reinterpret_cast <T *> (this);
    }
  };
  union
  {
    A p;
    A::B *d;
  };
  T & operator[](int i)
  {
    if (d->a != 1)
      f2 ();
    return reinterpret_cast <D *> (p.f1 (i))->f3 ();
  }
  void f2 ();
  void f3 (int i, const T & t);
};

class E
{
  int e, f;
};

C <E> c;

void
foo (int x)
{
  E e = c[x];
  c.f3 (x, e);
}
