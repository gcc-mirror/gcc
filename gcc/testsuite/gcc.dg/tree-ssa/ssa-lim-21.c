/* { dg-do compile  } */
/* { dg-options "-O2 -fdump-tree-lim2-details" } */

/* Test that `data' and 'data1' is not hoisted out of inner loop and outer loop
   when it is in cold loop.  */

int count;
volatile int x;

struct obj {
  int data;
  int data1;
  struct obj *next;
};

void
func (int m, int n, int k, struct obj *a)
{
  struct obj *q = a;
  for (int j = 0; j < n; j++)
    if (__builtin_expect (m, 0))
      for (int i = 0; i < m; i++)
	{
	  if (__builtin_expect (x, 0))
	    {
	      count++;
	      q->data += 3; /* Not hoisted out to inner loop. */
	    }
	  count += n;
	  q->data1 += k; /* Not hoisted out to outer loop. */
	}
}

/* { dg-final { scan-tree-dump "Executing store motion of count from loop 2" "lim2"  }  } */
/* { dg-final { scan-tree-dump "Executing store motion of \[^ \]*data1 from loop 2" "lim2"  }  } */
/* { dg-final { scan-tree-dump-times "Executing store motion of" 2 "lim2"  }  } */
