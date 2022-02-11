/* { dg-do compile  } */
/* { dg-options "-O2 -fdump-tree-lim2-details" } */

/* Test that `count' is not hoisted out of loop when bb is cold.  */

int count;
volatile int x;

struct obj {
  int data;
  struct obj *next;

} *q;

void
func (int m)
{
  struct obj *p;
  for (int i = 0; i < m; i++)
    if (__builtin_expect (x, 0))
      count++;

}

/* { dg-final { scan-tree-dump-not "Executing store motion of" "lim2"  }  } */
