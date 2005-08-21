/* { dg-do compile } */
/* { dg-options "-O2" } */

cblas_csyr2k (int N, void *A, int lda, float *B, int ldb, float *C, int k)
{
  int i, j;
  for (;; k ++)
    {
      for (i = 0; i < N; i ++)
	{
	  float t = ((float * ) A) [i];
	  for (j = i; j < N; j ++)
	    {
	      C [i + j] = B [ldb] * ((float *) A) [k];
	      C [lda] = 0 ;
	    }
	}
    }
}
