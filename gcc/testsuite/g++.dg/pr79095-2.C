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
    b.resize (b.size () - 1);
}

/* If b.size() == 0, then the argument to b.resize is -1U (it overflowed).
   This will result calling "fill" which turns into a memset with a bogus
   length argument.  We want to make sure we warn, which multiple
   things.  First the ldist pass converted the loop into a memset,
   cprop and simplifications made the length a constant and the static
   analysis pass determines it's a bogus size to pass to memset.  */
/* { dg-warning "exceeds maximum object size" "" { target *-*-* } 0 } */ 

