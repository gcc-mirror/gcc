/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

typedef long vec __attribute__ ((vector_size (2 * sizeof(long))));

void f (vec *r)
{
  vec a = { -2, 666 };
  vec b = { 3, 2 };
  *r = a < b;
}

/* { dg-final { scan-tree-dump-not "2, 666" "ccp1" } } */
