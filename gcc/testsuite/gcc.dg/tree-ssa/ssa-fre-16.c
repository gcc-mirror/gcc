/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

/* FRE should be able to combine i and j and perform simplification
   on the condition.  */

extern void link_error (void);
int i;
int foo(int b, int c)
{
  i = b + 1;
  int j = i - 1;
  if (b != j)
    link_error ();
}

/* { dg-final { scan-tree-dump-not "link_error" "fre1" } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
