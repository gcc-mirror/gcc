/* { dg-options "-O2 --param modref-max-accesses=2 -fdump-tree-modref1"  } */
/* { dg-do compile } */
void
test(char *a)
{
  a[0] = 0;
  a[1] = 1;
  a[3] = 3;
  a[7] = 7;
  a[9] = 9;
}
/* We allow only two accesses per function.
   It is best to group together {0,1,3} and {7,9}.  */
/* { dg-final { scan-tree-dump "access: Parm 0 param offset:0 offset:0 size:8 max_size:32" "modref1" } } */
/* { dg-final { scan-tree-dump "access: Parm 0 param offset:7 offset:0 size:8 max_size:24" "modref1" } } */
