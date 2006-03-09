// { dg-do compile }

extern void baz (int);

void foo (int j, int k)
{
  #pragma omp for
  for (int l = j; l < k; l++)
    baz (l);

  #pragma omp for
  for (int i = 0, m = 0; m < 10; m++)	// { dg-error "" }
    baz (m);
}
