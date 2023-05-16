/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-evrp -fdelete-null-pointer-checks" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */

extern void link_error ();

void foo (int *x)
{
  int *p = x + 1;
  if (p == 0)
    link_error ();
}

void bar (char *x, int a)
{
  if (a != 0)
    {
      char *p = x + a;
      if (p == 0)
	link_error ();
    }
}

/* { dg-final { scan-tree-dump-not "link_error" "evrp" } }  */
