// { dg-do run }
// { dg-additional-options "-fdump-tree-gimple" }

int
main (int argc, char **argv)
{
  int *ptr = 0;

  for (unsigned i = 0; i < 2; i++)
    {
      switch (argc)
	{
	int a;
      default:
	ptr = &a;
	*ptr = 12345;
	break;
      }
    }

  return 0;
}

/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(UNPOISON, &a, \[0-9\]\\);" 2 "gimple" } }  */
/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(POISON, &a, \[0-9\]\\);" 1 "gimple" } }  */
