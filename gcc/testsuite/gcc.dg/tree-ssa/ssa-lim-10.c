/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim1-details" } */

int *l, *r;
int test_func(void)
{
  int i;
  int direction;
  static int pos;

  pos = 0;
  direction = 1;

  for ( i = 0; i <= 400; i++ )
    {
      if ( direction == 0 )
	pos = l[pos];
      else
	pos = r[pos];

      if ( pos == -1 )
	{
	  pos = 0;
	  direction = !direction;
	}
    }
  return i;
}

/* { dg-final { scan-tree-dump "Executing store motion of pos" "lim1" } } */
/* { dg-final { cleanup-tree-dump "lim1" } } */
