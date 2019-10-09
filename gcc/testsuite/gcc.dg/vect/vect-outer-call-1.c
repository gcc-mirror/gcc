/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-fno-math-errno" } */

void
foo (float * __restrict x, float *y, int n, int m)
{
  if (m > 0)
    for (int i = 0; i < n; ++i)
      {
	float tem = x[i], tem1;
	for (int j = 0; j < m; ++j)
	  {
	    tem += y[j];
	    tem1 = tem;
	    tem = __builtin_sqrtf (tem);
	  }
	x[i] = tem - tem1;
      }
}

/* { dg-final { scan-tree-dump "OUTER LOOP VECTORIZED" "vect" { target { vect_call_sqrtf } } } } */
