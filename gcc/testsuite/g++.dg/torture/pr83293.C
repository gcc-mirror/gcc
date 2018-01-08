// PR tree-optimization/83293

typedef __SIZE_TYPE__ size_t;
template <typename T, typename> struct A {
  T a;
  A (T x) : a(x) {}
  T foo () { return a; }
};

template <typename T, typename U, typename V>
int
operator==(A<T, V> x, A<U, V> p2)
{
  return x.foo () == p2.foo ();
}

struct B { struct { int *b, *c; } d; };
struct C : B {
  A<int *, int> bar () { return d.b; }
  A<int *, int> baz () { return d.c; }
  size_t boo () { return d.c - d.b; }
  int zoo () { return bar () == baz (); }
};
struct D { C e; } a;
size_t b;

size_t
test (int x)
{
  size_t c (x * b);
  if (!a.e.zoo ())
    {
      x += 2;
      for (size_t d = 0, e = a.e.boo (); d < e; ++d)
	c += test (0);
    }
  c += (x - 1) * b;
  return c;
}
