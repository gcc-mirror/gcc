/* { dg-options "-O2 -fdump-tree-pre-stats" } */

void baz();
int tem;
void foo (int a, int b, int c, int d, int e, int x, int y, int z)
{
  if (a)
    {
      if (b)
        {
          if (c)
            {
	      if (d)
		{
		  if (e)
		    {
		      tem = x + y;
		    }
		  else
		    {
		      if (z) baz ();
		      tem = x + y;
		    }
		}
	      else
		{
		  if (z) baz ();
		  tem = x + y;
		}
	    }
          else
            {
              if (z) baz ();
              tem = x + y;
            }
        }
      else
        {
          if (z) baz ();
          tem = x + y;
        }
    }
  else
    {
      if (z) baz ();
      tem = x + y;
    }
}

/* Now inserting x + y five times is unnecessary but the cascading
   cannot be avoided with the simple-minded dataflow.  But make sure
   we do not iterate PRE insertion.  */
/* { dg-final { scan-tree-dump "insert iterations == 1" "pre" } } */
/* { dg-final { scan-tree-dump "HOIST inserted: 5" "pre" } } */
