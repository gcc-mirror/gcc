/* { dg-do compile } */
/* { dg-options "-Wall -O3" } */

typedef __SIZE_TYPE__ size_t;

struct S {
  int *p0, *p1, *p2;

  size_t size () const { return p1 - p0; }

  void f (size_t n) {
    if (n > size ())       // can't happen because
      foo (n - size ());   //   n is in [1, MIN(size() - 1, 3)]
    else if (n < size ())
      bar (p0 + n);
  }

  void foo (size_t n)
  {
    size_t left = (size_t)(p2 - p1);
    if (left >= n)
      __builtin_memset (p2, 0, n * sizeof *p2); // { dg-bogus "maximum object size" }

  }

  void bar (int*);
};

void f (S &s)
{
  size_t n = s.size ();
  if (n > 1 && n < 5)
    s.f (n - 1);
}
