/* { dg-do compile } */
/* { dg-options "-O3 -floop-nest-optimize -fdump-tree-graphite-details --param graphite-allow-codegen-errors=1" } */

int a[1];
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

/* { dg-final { scan-tree-dump-times "code generation error" 1 "graphite" } } */
