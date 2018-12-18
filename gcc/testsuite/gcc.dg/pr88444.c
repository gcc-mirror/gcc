/* PR tree-optimization/88444 */
/* { dg-do compile } */
/* { dg-options "-O1 -ftree-vrp -fno-tree-ccp -fno-tree-forwprop -fno-tree-fre" } */

int v;

int
foo (int, int);

static inline int
bar (long int x)
{
  return !!x ? x : 1;
}

static inline void
baz (int x)
{
  v += foo (0, 0) + bar (x);
}

void
qux (void)
{
  int a = 0;
  v = v || foo (0, 0);
  v = v || foo (0, 0);
  v = v || foo (0, 0);
  baz (a);
}
