/* { dg-options "-O3 -fgraphite-identity" } */

test_seg(int a, int b)
{
  int i,r=1;
  for(i=0; i<b ;i++)
    r*=a;
  return r;
}
