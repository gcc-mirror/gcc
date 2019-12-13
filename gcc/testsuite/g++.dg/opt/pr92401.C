// PR tree-optimization/92401
// { dg-do compile { target c++11 } }
// { dg-options "-O2" }

typedef float V __attribute__ ((__vector_size__ (4 * sizeof (float))));

V v;

void
foo ()
{
  int i;
  for (i = 0; i < 11; ++i)
    v = V { 0.0f, 0.0f, (float) i, 0.0f };
}
