/* { dg-do link } */
/* { dg-options "-O -fdump-tree-optimized" } */

extern void link_error (void);
void foo(int n)
{
  int * f = (int*) __builtin_malloc (n * sizeof (int));
  int * ff = (int*) __builtin_malloc (n * sizeof (int));
  int i;

  for (i = 0; i < n; ++i)
    {
      f[i] = 1;
      ff[i] = 2;
      if (f[i] != 1)
	link_error ();
      if (ff[i] != 2)
	link_error ();
    }

  __builtin_free (f);
  __builtin_free (ff);
}
int main()
{
  return 0;
}

/* We should have removed the calls to link_error () and all stores
   to the allocated memory.  */

/* { dg-final { scan-tree-dump-times "\\\*D" 0 "optimized" } } */
