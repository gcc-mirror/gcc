int
foo (void)
{
  int i, j, k, l;
  int B[4];
  int A[4][4][4][4];

  for (l = 0; l < 4; l++)
    {
      for (k = 0; k < 4; k++)
	{
	  for (j = 0; j < 4; j++)
	    {
	      for (i = 0; i < 2; i++)
		{
		  B[i] = A[i][k][j][l] + A[3 - i][k][j][l];
		  B[3 - i] = A[i][k][j][l] - A[3 - i][k][j][l];
		}
	      A[0][k][j][l] = B[0] + B[1];
	      A[2][k][j][l] = B[0] - B[1];
	      A[1][k][j][l] = B[3] + B[2];
	      A[3][k][j][l] = B[3] - B[2];
	    }

	  for (i = 0; i < 4; i++)
	    {
	      for (j = 0; j < 2; j++)
		{
		  B[j] = A[i][k][j][l] + A[i][k][3 - j][l];
		  B[3 - j] = A[i][k][j][l] - A[i][k][3 - j][l];
		}
	      A[i][k][0][l] = B[0] + B[1];
	      A[i][k][2][l] = B[0] - B[1];
	      A[i][k][1][l] = B[3] + B[2];
	      A[i][k][3][l] = B[3] - B[2];
	    }
	}
    }

  return A[0][1][0][2];
}

/* This should not be interchanged.  */
/* { dg-final { scan-tree-dump-times "will be interchanged" 0 "graphite" } } */ 
/* { dg-final { cleanup-tree-dump "graphite" } } */
