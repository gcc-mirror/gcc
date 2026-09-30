/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-evrp" } */

/* The index is unknown so the range for v_3 should be [0, 0][7, 7].  */

static const struct { int a; int b[4]; } tbl[2]
  = { { 1, { [3] = 5 } }, { 2, { 6, 7, 8, 9 } } };

int __GIMPLE (ssa,startwith("evrp"))
foo (int i)
{
  int v;

  __BB(2):
  v_3 = tbl[i_2(D)].b[1];
  return v_3;
}

/* { dg-final { scan-tree-dump "v_3 = .irange. int .0, 0..7, 7.\n" "evrp" } } */
