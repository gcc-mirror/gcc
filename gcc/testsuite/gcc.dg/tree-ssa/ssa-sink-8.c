/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-sink" } */

int foo(int *a, int r, short *b)
{
  int ret = 0;
  *a = 1;
  switch (r)
    {
      case 3:
	  *a = 5;
	  break;
      case 4:
      case 5:
	  *a = 9;
	  ret = r + 25;
	  break;
      default:
	  ret = r + 20;
    }
  *b = 9;
  return ret;
}

/* *a = 1 should be sunk into the default case.  */

/* { dg-final { scan-tree-dump-times "Sinking" 1 "sink" } } */
/* { dg-final { cleanup-tree-dump "sink" } } */
