/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1-details -fdump-tree-optimized" } */
/* PR tree-optimization/121418 */

struct s1
{
  unsigned char t[1024];
};

struct s1 f(struct s1 a)
{
  struct s1 removeme1 = a;
  __builtin_memcpy (&removeme1, &a, sizeof(struct s1));
  return removeme1;
}

/* { dg-final { scan-tree-dump-times "after previous" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "removeme1 " "optimized" } } */
