/* { dg-do compile } */
/* { dg-options "-fdump-tree-ccp1" } */

typedef unsigned vec __attribute__ ((vector_size (16)));
void
f (vec *a)
{
  vec s = { 5, 5, 5, 5 };
  *a = *a << s;
}

/* { dg-final { scan-tree-dump "<< 5" "ccp1" } } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
