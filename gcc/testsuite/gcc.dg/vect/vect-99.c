/* { dg-require-effective-target vect_int } */

int ca[100];

void foo (int n)
{
  unsigned int i;

  for (i = 0; i < n; i++)
    ca[i] = 2;
}

int main (void)
{
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
