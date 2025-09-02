/* { dg-do compile } */
/* { dg-options "-O -fno-tree-sra -fdump-tree-fre1" } */

struct s1
{
  int t;
};

struct s2
{
  struct s1 t;
};

int f1(int a)
{
  struct s1 t = (struct s1){a};
  struct s2 tt = (struct s2){t};
  struct s2 ttt = tt;
  struct s1 *t0 = &ttt.t;
  return t0->t;
}

/* { dg-final { scan-tree-dump "return a" "fre1" } } */
