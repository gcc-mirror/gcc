/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1-details -fdump-tree-optimized" } */
/* PR tree-optimization/121417 */

struct s1
{
  unsigned char t[1024];
};

struct s1 f(struct s1 a)
{
  struct s1 removeme1 = a;
  struct s1 removeme2;
  __builtin_memcpy (&removeme2, &removeme1, sizeof(struct s1));
  return removeme2;
}

/* { dg-final { scan-tree-dump-times "after previous" 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "removeme1 " "optimized" } } */
/* { dg-final { scan-tree-dump-not "removeme2 " "optimized" } } */
