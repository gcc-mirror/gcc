/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

void test(int* input, int* out, unsigned x1, unsigned x2)
{
  unsigned i, j;
  unsigned end = x1;

  for(i = j = 0; i < 1000; i++) {
      int sum = 0;
      end += x2;
      for( ; j < end; j++)
	sum += input[j];
      out[i] = sum;
  }
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
