/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

unsigned test()
{
  int arr[] = {5,4,3,2,1};
  int sum = 0;

  for(int i = 0;i < 5;++i)
    {
      for(int j = 0; j < 5; ++j)
	{
	  int t = arr[i];
	  arr[i] = arr[j];
	  arr[j] = t;
	}
    }

  for(int i = 0; i < 5; ++i)
    {
      sum += arr[i];
    }

  return sum;
}

/* { dg-final { scan-tree-dump "return 15;" "optimized" } } */
