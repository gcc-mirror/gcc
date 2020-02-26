/* PR tree-optimization/93820 */
/* { dg-do run } */
/* { dg-options "-O2 -fgimple" } */

typedef int v4si __attribute__((vector_size(4 * sizeof (int))));
int a[10];

void __GIMPLE (ssa,startwith("store-merging"))
foo (int *p)
{
  int _2;
  __BB(2):
  _2 = *p_1(D);
  a[6] = _2;
  a[4] = 1;
  __MEM <v4si> ((int *)&a + _Literal (int *) 16) = _Literal (v4si) { 0, 0, 0, 0 };
  a[3] = 0;
  return;
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
