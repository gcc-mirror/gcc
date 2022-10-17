/* PR100112 and dups.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1-details -fdump-tree-optimized" } */

int *c, *b;
void foo()
{
  int *tem = b;
  *tem = 0;
  int *footem = c;
  c = footem;
}

void bar()
{
  int *tem = b;
  int *bartem = c;
  *tem = 0;
  c = bartem;
}

/* We should elide the redundant store in foo, in bar it is not redundant since
   the *tem = 0 store might alias.  */
/* { dg-final { scan-tree-dump "Deleted redundant store c = footem" "fre1" } } */ 
/* { dg-final { scan-tree-dump "c = bartem" "optimized" } } */ 
