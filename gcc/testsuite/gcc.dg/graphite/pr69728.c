/* { dg-do compile } */
/* { dg-options "-O3 -floop-nest-optimize -fdump-tree-graphite-details" } */

int a[9];
int b, c, d, e;
void
fn1 ()
{
  d = 9;
  for (; c; c++)
    {
      ++d;
      b = 8;
      for (; b; b--)
	{
	  if (d)
	    break;
	  a[b] = e;
	}
    }
}

/* At the moment only ISL figures that if (d) is always true.  We've
   run into scheduling issues before here, not being able to handle
   empty domains.  */

/* { dg-final { scan-tree-dump "loop nest optimized" "graphite" } } */
