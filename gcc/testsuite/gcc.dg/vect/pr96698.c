/* { dg-do compile } */

void test(int a, int* i)
{
  for (; a < 5; ++a)
    {
      int b = 0;
      int c = 0;
      for (; b != -11; b--)
	for (int d = 0; d ==0; d++)
	  {
	    *i += c & a;
	    c = b;
	  }
    }
}

/* We should be able to vectorize the inner cycle.  */
/* { dg-final { scan-tree-dump "OUTER LOOP VECTORIZED" "vect" { target vect_int } } } */
