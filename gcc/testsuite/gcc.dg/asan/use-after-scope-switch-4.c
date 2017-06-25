// { dg-do run }
// { dg-additional-options "-fdump-tree-gimple" }

int *ptr;

struct a
{
  int c;
};

int main(int argc, char **argv)
{
  struct a e;
  e.c = 2;
  int x = 0;

  for (;;)
    switch (e.c)    
      case 3:
	{
	  int resxxx;
	case 2:
	  ptr = &resxxx;
	  *ptr = 123;

	  if (x)
	    return 0;
	  else
	    x = 1;
	}

  return 1;
}

/* { dg-final { scan-tree-dump-times "ASAN_MARK \\(UNPOISON, &resxxx, \[0-9\]\\);" 2 "gimple" } }  */
