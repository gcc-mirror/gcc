// PR c++/70571
// { dg-do compile }

typedef int V __attribute__ ((vector_size (sizeof (int))));

void
foo (V *x, V *y, int z)
{
  *x = (z == *y);
}
