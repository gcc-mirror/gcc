/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp" } */

void link_error (void);

void foo (int i)
{
  if (i > __INT_MAX__ - 10)
    {
      int j = i * 10;
      if (j < i)
	link_error ();
    }
}

/* { dg-final { scan-tree-dump-not "link_error" "evrp" } } */
