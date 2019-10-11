/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */

void test(double data[16][16])
{
  for (int i = 0; i < 16; i++)
    {
      for (int j = 0; j < i; j+=4)
	{
	  data[i][j] *= data[i][j];
	  data[i][j+1] *= data[i][j+1];
	  data[i][j+2] *= data[i][j+2];
	  data[i][j+3] *= data[i][j+3];
	}
    }
}

/* { dg-final { scan-tree-dump "Loop contains only SLP stmts" "vect" } } */
/* { dg-final { scan-tree-dump "ectorized 1 loops" "vect" } } */
