/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-alias1-vops" } */
/* Test to make sure that inline-asm causes a V_MAY_DEF. */


void link_error();
void f(char *a)
{
  int *a1 = (int *)a;
  if (*a == 0)
   asm("":"=m"(*a1));
  if (*a == 0)
   link_error ();
}

/* There should a V_MAY_DEF for the inline-asm and one for the link_error.  */
/* { dg-final { scan-tree-dump-times "V_MAY_DEF" 2 "alias1"} } */
