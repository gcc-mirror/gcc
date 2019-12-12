/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1" } */

typedef int v2si __attribute__((vector_size(__SIZEOF_INT__ * 2)));
int foo (int *a)
{
  a[0] = 1;
  a[1] = 2;
  v2si x = *(v2si *)a;
  *(v2si *)&a[2] = x;
  return a[3];
}

/* { dg-final { scan-tree-dump "return 2;" "fre1" } } */
