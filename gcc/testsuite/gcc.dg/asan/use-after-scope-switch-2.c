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

/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(UNPOISON, &a, \[0-9\]\\);" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(UNPOISON, &b, \[0-9\]\\);" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(UNPOISON, &c, \[0-9\]\\);" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(POISON, &a, \[0-9\]\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(POISON, &b, \[0-9\]\\);" 1 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(POISON, &c, \[0-9\]\\);" 1 "gimple" } }  */
