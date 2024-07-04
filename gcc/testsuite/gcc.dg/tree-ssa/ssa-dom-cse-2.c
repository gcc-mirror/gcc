/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-fre -fno-tree-pre -fdump-tree-optimized --param sra-max-scalarization-size-Ospeed=32" } */
/* System Z needs hardware vector support for this to work (the optimization
   gets too complex without it.
   { dg-additional-options "-march=z13" { target s390x-*-* } } */
/* Use generic tuning on x86 for the same reasons as why alpha, powerpc etc. are
   xfailed below.
   { dg-additional-options "-mtune=generic" { target i?86-*-* x86_64-*-* } } */

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

/* { dg-final { scan-tree-dump "return 28;" "optimized" { xfail { { alpha*-*-* hppa*64*-*-* nvptx*-*-* mmix-knuth-mmixware } || { { { lp64 && { powerpc*-*-* sparc*-*-* } } || aarch64_sve } || { arm*-*-* && { ! arm_neon } } } } } } } */
