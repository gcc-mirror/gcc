// { dg-do run }
// { dg-additional-options "-fdump-tree-gimple" }

int
main (int argc, char **argv)
{
  int *ptr = 0;
  int *ptr2 = 0;
  int *ptr3 = 0;

  for (unsigned i = 0; i < 2; i++)
    {
      switch (argc)
	{
	case 1111:;
	  int a, b, c;
	default:
	  ptr = &a;
	  ptr2 = &b;
	  ptr3 = &c;
	  break;
	}
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(2, &a, \[0-9\]\\);" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(2, &b, \[0-9\]\\);" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(2, &c, \[0-9\]\\);" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(1, &a, \[0-9\]\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(1, &b, \[0-9\]\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(1, &c, \[0-9\]\\);" 1 "gimple" } }  */
