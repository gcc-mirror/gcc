/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

int *ptr;
void link_error ();
void
test ()
{
  int *ptr1 = ptr + 10;
  int *ptr2 = ptr + 20;
  if (ptr1 == ptr2)
    link_error ();
}

/* { dg-final { scan-tree-dump-not "if" "fre1" } } */
