extern void dummy (int);

void
test3 (int x)
{
#pragma omp unroll full	/* { dg-error "non-constant iteration count of 'unroll full' loop" } */
  for (int i = x; i < 20; i += 3)
    dummy (i);
}

void
test4 (int x)
{
#pragma omp unroll full	/* { dg-error "non-constant iteration count of 'unroll full' loop" } */
  for (int i = 5; i < x + 6; ++i)
    dummy (i);
}

void
test5 (int x)
{
#pragma omp unroll full	/* { dg-error "non-constant iteration count of 'unroll full' loop" } */
  for (int i = 5; i < 142; i += x)
    dummy (i);
}
