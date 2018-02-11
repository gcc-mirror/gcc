// PR c++/83659
// { dg-do compile }

typedef int V __attribute__ ((__vector_size__ (16)));
V a;
V b[2];

int
foo ()
{
  return reinterpret_cast <int *> (&a)[-1] += 1;
}

int
bar ()
{
  return reinterpret_cast <int *> (&a[1])[-1];
}
