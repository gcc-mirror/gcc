/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */

void rescale_x4 (double* __restrict data,
		 const double* __restrict factor, int n)
{
  for (int i=0; i<n; i++)
    {
      data[4*i] *= factor[i];
      data[4*i+1] *= factor[i];
      data[4*i+2] *= factor[i];
      data[4*i+3] *= factor[i];
    }
}

void rescale_x4_s (double* __restrict data,
		   const double* __restrict factor, int n, int s)
{
  for (int i=0; i<n; i++)
    {
      data[s*i] *= factor[s*i];
      data[s*i+1] *= factor[s*i];
      data[s*i+2] *= factor[s*i];
      data[s*i+3] *= factor[s*i];
    }
}

/* All targets should be able to compose a vector from scalar elements, but
   depending on vector size different permutes might be necessary.  */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" { target vect_perm } } } */
/* { dg-final { scan-tree-dump-not "Data access with gaps requires scalar epilogue loop" "vect" { target vect_perm } } } */
