/* PR tree-optimization/93582 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O2 -fdump-tree-fre1" } */
/* { dg-final { scan-tree-dump "return 5364;" "fre1" { target le } } } */
/* { dg-final { scan-tree-dump "return 7838;" "fre1" { target be } } } */
/* { dg-final { scan-tree-dump "return 1959;" "fre1" { target le } } } */
/* { dg-final { scan-tree-dump "return 1268;" "fre1" { target be } } } */

union U { char a[8]; struct S { unsigned int b : 8, c : 13, d : 11; } e; } u;

__attribute__((noipa)) int
foo (void)
{
  __builtin_memset (&u.a, 0xf4, sizeof (u.a));
  return u.e.c;
}

__attribute__((noipa)) int
baz (void)
{
  __builtin_memset (&u.a, 0xf4, sizeof (u.a));
  return u.e.d;
}
