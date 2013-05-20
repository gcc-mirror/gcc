/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

typedef unsigned vec __attribute__ ((vector_size (4*sizeof(int))));
void
f (vec *a)
{
  vec s = { 5, 5, 5, 5 };
  *a = *a << s;
}

/* { dg-final { scan-tree-dump "<< 5" "ccp1" } } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
