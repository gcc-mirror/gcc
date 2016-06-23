/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */

struct omp_data_i
{
  double *__restrict__ results;
  double *__restrict__ pData;
  double *__restrict__ coeff;
};

#define nEvents 1000000

double __attribute__((noinline, noclone))
f (struct omp_data_i *__restrict__ p, int argc)
{

  int idx;

  for (idx = 0; idx < nEvents; idx++)
    ((p->results))[idx] = (*(p->coeff)) * ((p->pData))[idx];

  return ((p->results))[argc];
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump-not "versioned" "vect" } } */
