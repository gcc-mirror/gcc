/* PR tree-optimization/93820 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-tree-dse" } */

typedef int v4si __attribute__((vector_size(4 * sizeof (int))));
int a[10];

__attribute__((noipa)) int
foo (int *p)
{
  a[6] = *p;
  a[4] = 1;
  *(((v4si *)&a[0]) + 1) = (v4si) { 0, 0, 0, 0 };
  a[3] = 0;
}

int
main ()
{
  int i = 0;
  foo (&i);
  for (i = 0; i < 10; i++)
    if (a[i])
      __builtin_abort ();
  return 0;
}
