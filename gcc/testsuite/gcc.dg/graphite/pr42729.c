/* { dg-options "-O1 -fgraphite-identity -fcompare-debug" } */

int A[10];
int *foo ()
{
  int *p1, *p2, i;
  for (i = 0; i < 10; i++)
  {
    p1 = &A[i];
    *p1 = 0;
  }
  p2 = p1;
  return p2;
}
