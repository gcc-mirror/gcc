// PR tree-optimization/79734
// { dg-do compile }
// { dg-options "-O2" }
// { dg-additional-options "-mavx512vl" { target i?86-*-* x86_64-*-* } }

typedef float V __attribute__ ((vector_size (4 * sizeof (float))));

void
foo (V *a, V *b)
{
  *a = (*a < 1 && !(*b > 2)) ? *a + *b : 3;
}
