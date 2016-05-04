// PR c++/70933
// { dg-do compile }
// { dg-options "-Wsequence-point" }

struct A
{
  A (const char *);
};

template <class T>
struct B
{
  typedef T U;
  U &baz (const A &);
};

template <class T>
void
bar ()
{
  B<T> b;
  T &p = b.baz ("p1") = T(4);
}

void
foo ()
{
  bar<unsigned> ();
}
