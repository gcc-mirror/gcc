/* { dg-do compile } */
/* { dg-options "-fopenmp -O2 -fdump-tree-optimized -fno-tree-pre" } */

int
bar ()
{
  int a = 0, i;

#pragma omp parallel for num_threads (3) reduction (+:a) schedule(static, 1)
  for (i = 0; i < 10; i++)
    a += i;

  return a;
}

/* Two phis for reduction, one in loop header, one in loop exit.  One phi for iv
   in loop header.  */
/* { dg-final { scan-tree-dump-times "PHI" 3 "optimized" } } */
