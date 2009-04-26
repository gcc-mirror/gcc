/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

int  foo (int a)
{
void *p;
if (a!=0)
  p = &&L0;
else
  p = &&L1;
goto *p;
L0:
return 1;
L1:
return 0;
}

/* Everything should have been cleaned up leaving a simple
   return statement.  */
/* { dg-final { scan-tree-dump-times "= a_..D. != 0" 1 "optimized" } } */

/* There should not be any abnormal edges as DOM removed the
   computed gotos.  */

/* { dg-final { scan-tree-dump-times "ab" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
