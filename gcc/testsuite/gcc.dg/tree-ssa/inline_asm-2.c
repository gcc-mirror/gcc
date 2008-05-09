/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-alias-vops" } */
/* Test to make sure that inline-asm causes a VDEF. */


void link_error();
void f(char *a)
{
  int *a1 = (int *)a;
  if (*a == 0)
   asm("":"=m"(*a1));
  if (*a == 0)
   link_error ();
}

/* There should a VDEF for the inline-asm and one for the link_error.  */
/* { dg-final { scan-tree-dump-times "VDEF" 2 "alias"} } */
/* { dg-final { cleanup-tree-dump "alias" } } */
