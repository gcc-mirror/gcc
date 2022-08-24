/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* For negative stride we need to reverse the aligned load.  */
/* { dg-require-effective-target vect_perm } */

void b (int *p)
{
  p = (int *)__builtin_assume_aligned(p, __BIGGEST_ALIGNMENT__);
  int *q = p + 255;
  for(; p < q; ++p, --q)
    {
      int t = *p;
      *p = *q;
      *q = t;
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
