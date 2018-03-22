/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */

void test(double data[4][2])
{
  for (int i = 0; i < 4; i++)
    {
      data[i][0] = data[i][0] * data[i][0];
      data[i][1] = data[i][1] * data[i][1];
    }
}

/* We should vectorize this with SLP and V2DF vectors.  */
/* { dg-final { scan-tree-dump "vectorizing stmts using SLP" "vect" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
