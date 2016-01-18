/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-fre -fno-tree-pre -fdump-tree-optimized --param sra-max-scalarization-size-Ospeed=32" } */

int
foo ()
{
  const int a[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };
  int i, sum;

  sum = 0;
  for (i = 0; i < sizeof (a) / sizeof (*a); i++)
    sum += a[i];

  return sum;
}

/* After late unrolling the above loop completely DOM should be
   able to optimize this to return 28.  */

/* On alpha, the vectorizer generates writes of two vector elements at once,
   but the loop reads only one element at a time, and DOM cannot resolve these.
   The same happens on powerpc depending on the SIMD support available.  */

/* { dg-final { scan-tree-dump "return 28;" "optimized" { xfail alpha*-*-* powerpc64*-*-* } } } */
