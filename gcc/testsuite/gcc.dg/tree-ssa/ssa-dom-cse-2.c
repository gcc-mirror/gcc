/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-fre -fno-tree-pre -fdump-tree-optimized" } */

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

/* See PR63679 and PR64159, if the target forces the initializer to memory then
   DOM is not able to perform this optimization.  */

/* { dg-final { scan-tree-dump "return 28;" "optimized" { xfail aarch64*-*-* alpha*-*-* hppa*-*-* powerpc*-*-* sparc*-*-* s390*-*-* } } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
