/* { dg-options "-O2 --param modref-max-accesses=1 -fdump-tree-modref1"  } */
/* { dg-do compile } */
struct a {
  int array[10];
  int tail;
};
int test(struct a *a, int p)
{
  a->array[p] = 0;
  a->array[0] = 1;
}
/* All three accesses combine to one bigger access.  */
/* { dg-final { scan-tree-dump-not "param=modref-max-accesses" "modref1" } } */
