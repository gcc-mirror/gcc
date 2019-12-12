/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

typedef int v4si __attribute__((vector_size(__SIZEOF_INT__ * 4)));
int foo (int *a)
{
  a[2] = 2;
  a[0] = 0;
  a[1] = 1;
  a[3] = 4;
  v4si x = *(v4si *)a;
  *(v4si *)&a[4] = x;
  return a[4] + a[7];
}

/* { dg-final { scan-tree-dump "return 4;" "fre1" } } */
