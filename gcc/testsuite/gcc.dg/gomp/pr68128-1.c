/* PR tree-optimization/68128 */
/* { dg-do compile } */
/* { dg-options "-Ofast -fopenmp -fdump-tree-vect-details" } */
/* { dg-additional-options "-mavx" { target i?86-*-* x86_64-*-* } } */

/* Make sure the following loop is vectorized even when not using
   firstprivate variables for scalar vars that are not modified
   in the parallel region.  */

void
foo (float *u, float v, float w, float x, float y, float z, float t)
{
  int i, j, k, l;
  float a, *b, c, s, e;
#pragma omp parallel for private (i, j, k, l, a, b, c, s, e)
  for (j = 0; j < 1024; j++)
    {
      k = j * 64;
      l = j * 64 + 63;
      a = v + j * w;
      b = u + j * 64;
      for (i = k; i <= l; i++, b++, a += w)
	{
	  c = a * a + y;
	  s = (1.f - c * x) * (1.f - c * x);
	  e = t * (1 / __builtin_sqrtf (c)) * s;
	  *b += (c < z ? e : 0);
	}
    }
}

/* { dg-final { scan-tree-dump "note: vectorized 1 loops in function" "vect" { target i?86-*-* x86_64-*-* } } } */
