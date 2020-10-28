/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */

double x[1024];
void bar (void);

void foo (void)
{
  double tem1 = x[0];
  double tem2 = x[1];
  for (int i = 0; i < 511; ++i)
    {
      x[2*i] = tem1;
      x[2*i+1] = tem2;
      bar ();
      tem1 = x[2*(i+1)];
      tem2 = x[2*(i+1)+1];
    }
}

/* We should be able to vectorize the cycle in one SLP attempt including
   both load groups.  */
/* { dg-final { scan-tree-dump-times "transform load" 2 "slp1" } } */
/* { dg-final { scan-tree-dump-times "optimized: basic block" 1 "slp1" } } */
