/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1" } */

/* We should be able to fold the comparison at least with the
   first forwprop pass, if not a ccp pass before.  */

extern void link_error (void);
void foo()
{
  int i;
  char *p = (char *)&i;
  long *q = (long *)p;
  if (q == 0)
    link_error ();
}

/* { dg-final { scan-tree-dump-not "link_error" "forwprop1" } } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */
