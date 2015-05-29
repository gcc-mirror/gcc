/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

typedef int vec __attribute__ ((vector_size (4 * sizeof (int))));

void f (vec *r)
{
  vec a = { 2, 3, 4, 5 };
  *r = (a << 2) >> 1;
}

/* { dg-final { scan-tree-dump "{ 4, 6, 8, 10 }" "ccp1"} } */
