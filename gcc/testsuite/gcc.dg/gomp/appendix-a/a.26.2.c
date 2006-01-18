/* { dg-do compile } */
/* { dg-options "-fopenmp -std=c99" } */

int a;
void
g (int k)
{
  a = k;			/* The global "a", not the private "a" in f */
}

void
f (int n)
{
  int a = 0;
#pragma omp parallel for private(a)
  for (int i = 1; i < n; i++)
    {
      a = i;
      g (a * 2);		/* Private copy of "a" */
    }
}
