/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

int a[1024];
int b[1024], c[1024];
void foo ()
{
  for (int j = 0; j < 1024; ++j)
    {
      for (int i = 0; i < 1024; ++i)
	a[i] = j;
      b[j] = c[j];
    }
}

/* We should not hoist/PRE the outer loop IV increment or the load
   from c across the inner loop.  */

/* { dg-final { scan-tree-dump-not "HOIST inserted" "pre" } } */
