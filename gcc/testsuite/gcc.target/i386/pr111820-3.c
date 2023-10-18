/* { dg-do compile } */
/* { dg-options "-O3 -mavx2 -fno-tree-vrp -fdump-tree-vect-details -Wno-aggressive-loop-optimizations" } */
/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */

int r;
int r_0;

void f (void)
{
  int n = 14;
  while (-- n)
    {
      r_0 += r ;
      r  *= 3;
    }
}
