/* { dg-do compile } */
/* { dg-additional-options "--param ix86-vect-compare-costs=1" } */

void foo (int *block)
{
  for (int i = 0; i < 3; ++i)
    {
      int a = block[i*9];
      int b = block[i*9+1];
      block[i*9] = a + 10;
      block[i*9+1] = b + 10;
    }
}

/* { dg-final { scan-tree-dump "optimized: loop vectorized using 8 byte vectors" "vect" } } */
