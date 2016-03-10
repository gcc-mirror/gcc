/* { dg-do compile } */
/* { dg-require-effective-target fopenmp } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fopenmp -fipa-pta" } */

extern void abort (void);

#define nEvents 1000

static void __attribute__((noinline, noclone, optimize("-fno-tree-vectorize")))
init (unsigned *results, unsigned *pData)
{
  unsigned int i;
  for (i = 0; i < nEvents; ++i)
    pData[i] = i % 3;
}

static void __attribute__((noinline, noclone, optimize("-fno-tree-vectorize")))
check (unsigned *results)
{
  unsigned sum = 0;
  for (int idx = 0; idx < (int)nEvents; idx++)
    sum += results[idx];

  if (sum != 1998)
    abort ();
}

int
main (void)
{
  unsigned results[nEvents];
  unsigned pData[nEvents];
  unsigned coeff = 2;

  init (&results[0], &pData[0]);

#pragma omp parallel for
  for (int idx = 0; idx < (int)nEvents; idx++)
    results[idx] = coeff * pData[idx];

  check (&results[0]);

  return 0;
}

/* { dg-final { scan-tree-dump-times "note: vectorized 1 loop" 1 "vect" { xfail { vect_no_align && { ! vect_hw_misalign } } } } } */
/* { dg-final { scan-tree-dump-not "versioning for alias required" "vect" } } */

