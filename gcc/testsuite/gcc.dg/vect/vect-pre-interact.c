/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */

/* This checks that PRE doesn't create situations that prevent vectorization.
   I.e. PR39300, PR35229.  */
float res[1024], data[1025];

void foo (void)
{
  int i;
  for (i = 0; i < 1024; ++i)
    res[i] = data[i] + data[i + 1];
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { xfail vect_no_align } } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
