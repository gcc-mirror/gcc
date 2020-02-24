/* PR tree-optimization/93582 */
/* { dg-do compile { target int32 } } */
/* { dg-options "-O2 -fdump-tree-fre1" } */
/* { dg-final { scan-tree-dump "return 0;" "fre1" { target le } } } */
/* { dg-final { scan-tree-dump "return -8531;" "fre1" { target be } } } */

short
foo (void)
{
  union U { char c[32]; short s[16]; int i[8]; } u;
  __builtin_memset (u.c + 1, '\0', 5);
  u.s[3] = 0xdead;
  return u.i[1];
}
