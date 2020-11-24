/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

struct f
{
  struct {
      int i;
  } ff[10];
};

struct f g;
int ffff(int i)
{
  int t1 = 0;
  int i1 = g.ff[t1].i;
  int i2 = g.ff[i].i;
  return i1 + i2;
}

/* We should not use extra temporaries apart from for i1 + i2.  */

/* { dg-final { scan-tree-dump-times "int" 6 "gimple" } } */
/* { dg-final { scan-tree-dump-times "int D\\\." 1 "gimple" } } */
