/* Test normalization of ARRAY_REF expressions to MEM_REFs in dom.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-fre -fdump-tree-dom2" } */

#define N 8

int
main (int argc, char **argv)
{
  int a[N];
  for (int i = 0; i < N; i++)
    a[i] = 2*i + 1;
  int *p = &a[0];
  __builtin_printf ("%d\n", a[argc]);
  return *(++p);
}

/* { dg-final { scan-tree-dump-times "return 3;" 1 "dom2"} } */
