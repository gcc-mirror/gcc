/* { dg-do compile } */
/* { dg-options "-O2 -march=sierraforest -fdump-tree-vect-details" } */
/* { dg-final { scan-tree-dump "loop vectorized using 16 byte vectors" "vect" } } */

int a[256], b[256];

void __attribute__((noinline))
foo (void)
{
  int i;
  for (i = 0; i < 32; ++i)
    {
      b[i*8+0] = a[i*8+0];
      b[i*8+1] = a[i*8+0];
      b[i*8+2] = a[i*8+3];
      b[i*8+3] = a[i*8+3];
      b[i*8+4] = a[i*8+4];
      b[i*8+5] = a[i*8+6];
      b[i*8+6] = a[i*8+4];
      b[i*8+7] = a[i*8+6];
    }
}
