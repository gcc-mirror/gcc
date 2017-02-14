/* { dg-do compile } */
/* { dg-options "-Wall -O3" } */

typedef long unsigned int size_t;

inline void
fill (int *p, size_t n, int)
{
  while (n--)
    *p++ = 0;
}

struct B
{
  int* p0, *p1, *p2;

  size_t size () const {
    return size_t (p1 - p0);
  }

  void resize (size_t n) {
    if (n > size())
      append (n - size());
  }

  void append (size_t n)
  {
    if (size_t (p2 - p1) >= n) 	 {
      fill (p1, n, 0);
    }
  }
};

void foo (B &b)
{
  if (b.size () != 0)
    b.resize (b.size () - 1);
}


