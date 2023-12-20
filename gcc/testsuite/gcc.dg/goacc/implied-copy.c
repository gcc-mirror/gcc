/* { dg-additional-options "-fdump-tree-gimple" } */

/* Test for float _Complex and double _Complex types.  */
int
main(void)
{
  int i;
  float _Complex fc[100];
  float _Complex s1;
  double _Complex dc[100];
  double _Complex s2;

#pragma acc parallel reduction (+:s1, s2)
  for (i = 0; i < 10; i++)
    {
      s1 += fc[i];
      s2 += dc[i];
    }
#pragma acc serial reduction (+:s1, s2)
  for (i = 0; i < 10; i++)
    {
      s1 += fc[i];
      s2 += dc[i];
    }
  return 0;
}

/* { dg-final { scan-tree-dump-times "map\\(tofrom:s1 \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\(tofrom:s2 \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } */
