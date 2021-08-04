/* { dg-do compile } */

__attribute__ ((simd)) int
tq (long int ea, int of, int kk)
{
  int bc;

  for (bc = 0; bc < 2; ++bc)
    {
      ++ea;
      of |= !!kk < !!ea;
    }

  return of;
}
