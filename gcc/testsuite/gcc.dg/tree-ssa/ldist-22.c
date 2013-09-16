/* { dg-do run } */
/* { dg-options "-O3 -fdump-tree-ldist-details" } */

extern void abort (void);

int a[1024], b[1024];

void __attribute__((noinline,noclone))
foo (void)
{
  int i;
  for (i = 0; i < 1024; ++i)
    {
      a[i] = 0;
      if (i > 100)
	b[i] = i;
    }
}

int main()
{
  b[100] = 1;
  foo ();
  if (b[100] != 1 || b[101] != 101)
    abort ();
  if (a[0] != 0 || a[101] != 0)
    abort ();
  return;
}

/* { dg-final { scan-tree-dump "generated memset zero" "ldist" } } */
/* { dg-final { cleanup-tree-dump "ldist" } } */
