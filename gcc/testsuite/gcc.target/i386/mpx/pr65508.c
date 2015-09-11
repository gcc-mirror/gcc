/* { dg-do compile } */
/* { dg-options "-O2 -fcheck-pointer-bounds -mmpx" } */

void
bar (int N)
{
  int a[N];
  void foo (int a[N])
  {
  }
  foo (a);
}
