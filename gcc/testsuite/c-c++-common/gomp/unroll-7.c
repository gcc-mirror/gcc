extern void dummy (int);
int a[100];

void
test1 (void)
{
#pragma omp unroll full
  for (int i = -20; i < 20; i += 6)
    dummy (i);
}

void
test2 (void)
{
#pragma omp unroll full
  for (int *i = &a[6]; i < &a[78]; i += 4)
    dummy (*i);
}
