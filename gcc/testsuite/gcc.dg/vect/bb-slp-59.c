/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */
/* { dg-additional-options "-fdump-tree-loopdone" } */

double x[1024];
void bar (void);

void foo (void)
{
  double tem1 = x[0];
  double tem2 = x[1];
  for (int i = 0; i < 511; ++i)
    {
      x[2*i] = tem2;
      x[2*i+1] = tem1;
      bar ();
      tem1 = x[2*(i+1)];
      tem2 = x[2*(i+1)+1];
    }
}

/* We should be able to vectorize the cycle in one SLP attempt including
   both load groups and do only one permutation.  */
/* { dg-final { scan-tree-dump-times "transform load" 2 "slp1" } } */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 1 "loopdone" } } */
/* { dg-final { scan-tree-dump-times "optimized: basic block" 1 "slp1" } } */
