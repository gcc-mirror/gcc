/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O1 -fdump-tree-lim1" } */

/* Test that thread visible loads do not get hoisted out of loops if
   the load would not have occurred on each path out of the loop.  */

int x[10] = {0,0,0,0,0,0,0,0,0,0};
int DATA_DATA = 0;

void reader()
{
  int i;
  __transaction_atomic
    { 
      for (i = 0; i < 10; i++)
        if (x[i])
          x[i] += DATA_DATA;
      /* If we loaded DATA_DATA here, we could hoist it before the loop,
	 but since we don't... we can't.  */
    }
}

/* { dg-final { scan-tree-dump-times "Cannot hoist.*DATA_DATA because it is in a transaction" 1 "lim1" } } */
/* { dg-final { cleanup-tree-dump "lim1" } } */
