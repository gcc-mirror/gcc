/* pr39500: autopar fails to parallel */
/* origin: nemokingdom@gmail.com(LiFeng) */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=4 -fdump-tree-parloops2-details" } */

void abort (void);

int main (void)
{
  int i;
  int x[100000];

  for (i = 0; i < 10000; i++)
    x[i] = x[i+10000];

  for (i = 0; i < 10000; i++)
    {
      if (x[i] != x[i+10000])
       abort ();
    }

  return 0;
}

/* Check that the first loop in parloop got parallelized.  */

/* { dg-final { scan-tree-dump-times "SUCCESS: may be parallelized" 1 "parloops2" } } */
