/* { dg-do run } */
/* { dg-options "-ffast-math -fdump-tree-gimple" } */

extern double sqrt (double);
extern double pow (double, double);
extern void abort (void);

int main ()
{
  double x = -1.0;
  if (sqrt (pow (x, 2)) != 1.0)
    abort();
  if (sqrt (x*x) != 1.0)
    abort();
  return 0;
}

/* { dg-final { scan-tree-dump-times "sqrt" 0 "gimple" } } */
/* { dg-final { scan-tree-dump-times "pow" 0 "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
